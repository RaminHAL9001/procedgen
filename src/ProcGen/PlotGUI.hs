-- | GUI for plotting functions, mostly instructions on how to redraw
-- and how to trace a mouse cursor along a plot.
module ProcGen.PlotGUI
  ( module ProcGen.PlotGUI
  , module ProcGen.Plot
  ) where

import           ProcGen.Plot

import           Control.Arrow
import           Control.Lens

import qualified Graphics.Rendering.Cairo as Cairo

import           Happlets.Lib.Gtk

import           Linear.V2

import           Text.Printf

----------------------------------------------------------------------------------------------------

clearWithBGColor :: PackedRGBA32 -> Cairo.Render ()
clearWithBGColor c = do
  let (r,g,b,a) = unpackRGBA32Color c
  cairoClearCanvas r g b a

setLineStyle :: (Real num, Fractional num) => LineStyle num -> Cairo.Render ()
setLineStyle style = do
  let c = theLineColor style
  let (r,g,b,a) = unpackRGBA32Color c
  Cairo.setLineJoin Cairo.LineJoinRound
  Cairo.setLineWidth $ realToFrac $ theLineWeight style
  Cairo.setSourceRGBA r g b a

drawPlotAxis
  :: forall num . (Real num, Fractional num)
  => SampCoord -> PlotAxis num -> (Double -> Cairo.Render ()) -> Cairo.Render ()
drawPlotAxis win axis draw = do
  win <- pure (realToFrac win :: Double)
  let (lo, hi) = realToFrac . uncurry min &&& realToFrac . uncurry max $
        (thePlotAxisMax axis, thePlotAxisMin axis) :: (Double, Double)
  let plotWin = hi - lo
  let scale = win / plotWin -- Multiply to convert plot coords to window coords.
  let off = realToFrac (thePlotAxisOffset axis) :: Double
  let toWinCoords  x = scale * (x - off) :: Double
  let stepLimit = 3.5 :: Double -- Any less and the grid basically becomes a solid color.
  let drawAll lines = do
        let step = realToFrac (theGridLinesSpacing lines :: num) :: Double
        unless (plotWin == 0 || step * scale < stepLimit) $ do
          let exactLinesBelowMin = lo / step :: Double
          let linesBelowMin = floor exactLinesBelowMin :: Int
          let initOff = exactLinesBelowMin - realToFrac linesBelowMin
          setLineStyle $ theGridLinesStyle lines
          mapM_ (draw . toWinCoords) $ takeWhile ((< win) . toWinCoords) $ iterate (+ step) initOff
  drawAll $ thePlotAxisMajor axis
  maybe (return ()) drawAll $ thePlotAxisMinor axis

drawPlotWindow :: (Real num, Fractional num) => PlotWindow num -> PixSize -> Cairo.Render ()
drawPlotWindow plotwin (V2 w h) = do
  drawPlotAxis h (theYAxis plotwin) $ (+ 0.5) >>> \ y ->
    Cairo.moveTo  0.5    y  >> Cairo.lineTo  (realToFrac w)  y >> Cairo.stroke
  drawPlotAxis w (theXAxis plotwin) $ (+ 0.5) >>> \ x ->
    Cairo.moveTo    x  0.5  >> Cairo.lineTo  x  (realToFrac h) >> Cairo.stroke

renderCartesian :: forall num . RealFrac num => PlotCartesian num -> PixSize -> CairoRender ()
renderCartesian plot size@(V2 (SampCoord w) (SampCoord h)) = cairoRender $ do
  let plotwin = plot ^. plotWindow
  let xaxis = plotwin ^. xAxis
  let yaxis = plotwin ^. yAxis
  let (xlo, xhi) = (xaxis ^. plotAxisMax, xaxis ^. plotAxisMin)
  let (ylo, yhi) = (yaxis ^. plotAxisMax, yaxis ^. plotAxisMin)
  let xwin = xhi - xlo
  let ywin = yhi - ylo
  let xscale = xwin / realToFrac w
  let yscale = ywin / realToFrac h
  let xtrans = (+ xlo) . (* xscale)
  let ytrans = (/ yscale) . subtract ylo
  let drawAxis isAbove = do
        isAbove (xaxis ^. plotAxisAbove) $ drawPlotWindow plotwin size
        isAbove (yaxis ^. plotAxisAbove) $ drawPlotWindow plotwin size
  clearWithBGColor (plot ^. plotWindow ^. bgColor)
  drawAxis unless
  when (xwin /= 0 && ywin /= 0) $ forM_ (plot ^. plotFunctionList) $ \ cart -> do
    let f = ytrans . (cart ^. cartFunction) . xtrans
    let xstep = realToFrac w / xwin
    setLineStyle (cart ^. lineStyle)
    let x0 = (if xstep < 0 then realToFrac w else 0) :: num
    let y0 = f (realToFrac x0) :: num
    Cairo.moveTo (realToFrac x0) (realToFrac y0)
    forM_ (if xstep < 0 then [w, w-1 .. 0] else [0 .. w]) $ \ x -> do
      Cairo.lineTo (realToFrac x) (realToFrac $ f $ realToFrac x)
    Cairo.stroke
  drawAxis when

resizeCart :: RealFrac num => GtkGUI (PlotCartesian num) ()
resizeCart = renderCartesian <$> getModel <*> getWindowSize >>= onCanvas

runCartesian :: forall num . RealFrac num => GtkGUI (PlotCartesian num) ()
runCartesian = do
  resizeEvents $ const resizeCart
  mouseEvents MouseAll $ \ mouse@(Mouse _ press1 _mod1 _button1 pt1@(V2 x1 _y1)) -> do
    funcList <- use plotFunctionList :: GtkGUI (PlotCartesian num) [Cartesian num]
    cursor   <- use $ plotWindow . lastMouseLocation
    plotWindow . lastMouseLocation .= Just mouse
    winsize@(V2 _ winH) <- getWindowSize
    let maxLineWidth = round $ maximum $ view lineWeight <$> funcList :: SampCoord
    let clearRegion (V2 x0 _y0) = unless (null funcList) $ refreshRegion
          [ rect2D &~ do
              rect2DHead .= V2 (x0 - maxLineWidth - 1) 0
              rect2DTail .= V2 (x0 + maxLineWidth + 2) winH
          ]
    let negColor = const black -- TODO: negate color, set alpha channel to 1.0
    let drawGuideLine plotwin = unless (null funcList) $
          drawLine (negColor $ plotwin ^. bgColor) 1.0 $ line2D &~ do
            let x = realToFrac x1 + 0.5
            line2DHead .= V2 x 0
            line2DTail .= V2 x (realToFrac winH)
    let drawPoints plotwin winsize = forM funcList $ \ func -> do
          let wp2pp = winPointToPlotPoint plotwin winsize :: Iso' (SampCoord, SampCoord) (num, num)
          let pp2wp = from wp2pp :: Iso' (num, num) (SampCoord, SampCoord)
          let (x, _y) = pt1 ^. pointXY . wp2pp :: (num, num)
          let y = theCartFunction (func :: Cartesian num) x
              -- HERE ^ is where the plot function is evaluaed
          let cairoCoord = (+ 0.5) . realToFrac :: SampCoord -> Double
          let (xp, yp) = cairoCoord *** cairoCoord $ (x, y) ^. pp2wp
          cairoRender $ do
            Cairo.setOperator Cairo.OperatorSource
            Cairo.arc xp yp (realToFrac maxLineWidth) 0 (2*pi)
            cairoSetColorRGBA32 $ plotwin ^. bgColor
            Cairo.fillPreserve
            cairoSetColorRGBA32 $ func ^. lineColor
            Cairo.setLineWidth $ realToFrac $ func ^. lineWeight
            Cairo.stroke
          return (x, y)
    case cursor of
      Nothing -> return ()
      Just (Mouse _ press0 _mod0 _button0 pt0) ->
        if press0 && press1 -- if this is a mouse-drag
         then do
          plotOrigin %= subtract (fmap realToFrac $ pt0 - pt1)
          resizeCart
         else clearRegion pt0
    plotwin <- use plotWindow
    onOSBuffer $ do
      drawGuideLine plotwin
      points <- drawPoints plotwin winsize
      screenPrinter $ do
        textCursor . gridRow    .= 0
        textCursor . gridColumn .= 0
        displayString $ do
          (x, y) <- points
          (printf "x = %+.3f\ny = %+.3f" (realToFrac x :: Float) (realToFrac y :: Float))
  resizeCart
