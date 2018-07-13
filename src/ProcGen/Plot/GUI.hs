-- | GUI for plotting functions, mostly instructions on how to redraw
-- and how to trace a mouse cursor along a plot.
module ProcGen.Plot.GUI
  ( module ProcGen.Plot.GUI
  , module ProcGen.Plot
  ) where

import           ProcGen.Plot
import           ProcGen.Types

import           Control.Arrow
import           Control.Lens

import qualified Graphics.Rendering.Cairo as Cairo

import           Happlets.Lib.Gtk

import           Linear.V2

import           Text.Printf

----------------------------------------------------------------------------------------------------

clearWithBGColor :: Color -> Cairo.Render ()
clearWithBGColor c = do
  let (r,g,b,a) = unpackRGBA32Color c
  cairoClearCanvas r g b a
{-# SPECIALIZE resizeCart :: GtkGUI (PlotCartesian ProcGenFloat) () #-}

setLineStyle :: (Real num, Fractional num) => LineStyle num -> Cairo.Render ()
setLineStyle style = do
  let c = theLineColor style
  let (r,g,b,a) = unpackRGBA32Color c
  Cairo.setLineJoin Cairo.LineJoinRound
  Cairo.setLineWidth $ realToFrac $ theLineWeight style
  Cairo.setSourceRGBA r g b a
{-# SPECIALIZE setLineStyle :: LineStyle ProcGenFloat -> Cairo.Render () #-}

drawPlotWindow :: RealFrac num => PlotWindow num -> PixSize -> Cairo.Render ()
drawPlotWindow plotwin size@(V2 w h) = do
  let lim = canonicalRect2D $ winToPlotRect plotwin size
  let wp2ppX = fromPlotToWinX plotwin size
  let wp2ppY = fromPlotToWinY plotwin size
  let steps wp2pp gap lo hi =
        let s = lo / gap
            i = realToFrac (floor s :: Int) * gap
        in  fmap wp2pp $ takeWhile (<= hi) $ iterate (gap +) i
  let vertical   ymax x = do
        Cairo.moveTo (realToFrac x + 0.5) (0.0)
        Cairo.lineTo (realToFrac x + 0.5) (realToFrac ymax)
        Cairo.stroke
  let horizontal xmax y = do
        Cairo.moveTo (0.0)             (realToFrac y + 0.5)
        Cairo.lineTo (realToFrac xmax) (realToFrac y + 0.5)
        Cairo.stroke
  let drawLines paint wmax wp2pp axis point = do
        let run majmin = case plotwin ^. axis . majmin of
              Nothing  -> return ()
              Just gap -> do
                setLineStyle $ gap ^. lineStyle
                mapM_ (paint wmax) $ steps wp2pp
                  (gap ^. gridLinesSpacing) (lim ^. rect2DHead . point) (lim ^. rect2DTail . point)
        run (plotAxisMajor . re _Just) >> run plotAxisMinor
  drawLines horizontal w wp2ppY yAxis pointY
  drawLines vertical   h wp2ppX xAxis pointX
  
{-# SPECIALIZE drawPlotWindow :: PlotWindow ProcGenFloat -> PixSize -> Cairo.Render () #-}

renderCartesian :: forall num . RealFrac num => PlotCartesian num -> PixSize -> CairoRender ()
renderCartesian plot size@(V2 w _h) = cairoRender $ do
  let plotwin = plot ^. plotWindow
  let drawAxis isAbove = do
        isAbove (plot ^. xAxis . plotAxisAbove) $ drawPlotWindow plotwin size
        isAbove (plot ^. yAxis . plotAxisAbove) $ drawPlotWindow plotwin size
  clearWithBGColor (plot ^. plotWindow ^. bgColor)
  drawAxis unless
  let sc2d  = (+ 0.5) . (realToFrac :: Int -> Double) . (fromIntegral :: SampCoord -> Int)
  let wp2pp =       winToPlotPoint plotwin size  :: Iso' (SampCoord, SampCoord) (num, num)
  let pp2wp = from (winToPlotPoint plotwin size) :: Iso' (num, num) (SampCoord, SampCoord)
  let undefined_y = error "renderCartesian: winToPlotPoint evaluated unused Y value"
  let xOnly x = fst $ (x :: SampCoord, undefined_y :: SampCoord) ^. wp2pp
  forM_ (reverse $ plot ^. plotFunctionList) $ \ cart -> do
    let f = theCartFunction cart
    case (sc2d *** sc2d) . view pp2wp . (id &&& f) . xOnly <$> [0 .. w] of
      []              -> return ()
      (x0, y0):points -> do
        cairoSetColor $ cart ^. lineColor
        Cairo.setLineWidth $ realToFrac $ cart ^. lineWeight
        Cairo.moveTo x0 y0
        forM_ points $ uncurry Cairo.lineTo
        Cairo.stroke
  drawAxis when
{-# SPECIALIZE renderCartesian :: PlotCartesian ProcGenFloat -> PixSize -> CairoRender () #-}

resizeCart :: RealFrac num => GtkGUI (PlotCartesian num) ()
resizeCart = renderCartesian <$> getModel <*> getWindowSize >>= onCanvas

----------------------------------------------------------------------------------------------------

runCartesian :: forall num . RealFrac num => GtkGUI (PlotCartesian num) ()
runCartesian = do
  resizeEvents $ const resizeCart
  mouseEvents MouseAll $ \ mouse1@(Mouse _ press1 _mod1 _button1 pt1@(V2 x1 _y1)) -> do
    funcList <- use plotFunctionList :: GtkGUI (PlotCartesian num) [Cartesian num]
    winsize@(V2 _ winH) <- getWindowSize
    let maxLineWidth = round $ maximum $ view lineWeight <$> funcList :: SampCoord
    let clearRegion (V2 x0 _y0) = unless (null funcList) $ refreshRegion
          [ rect2D &~ do
              rect2DHead .= V2 (x0 - maxLineWidth - 1) 0
              rect2DTail .= V2 (x0 + maxLineWidth + 2) winH
          ]
    let negColor = const black -- TODO: negate color, set alpha channel to 1.0
    plotwin <- use plotWindow
    let wp2pp =       winToPlotPoint plotwin winsize  :: Iso' (SampCoord, SampCoord) (num, num)
    let pp2wp = from (winToPlotPoint plotwin winsize) :: Iso' (num, num) (SampCoord, SampCoord)
    let (x, y) = (pt1 ^. pointXY) ^. wp2pp :: (num, num)
    let drawGuideLine = unless (null funcList) $
          drawLine (negColor $ plotwin ^. bgColor) 1.0 $ line2D &~ do
            let x = realToFrac x1 + 0.5
            line2DHead .= V2 x 0
            line2DTail .= V2 x (realToFrac winH)
    let drawGuidePoints = forM (reverse funcList) $ \ func -> do
          let y = theCartFunction (func :: Cartesian num) x
              -- HERE ^ is where the plot function is evaluaed
          let cairoCoord = (+ 0.5) . realToFrac :: SampCoord -> Double
          let (xp, yp) = cairoCoord *** cairoCoord $ (x, y) ^. pp2wp
          cairoRender $ do
            Cairo.setOperator Cairo.OperatorSource
            Cairo.arc xp yp (realToFrac maxLineWidth) 0 (2*pi)
            cairoSetColor $ plotwin ^. bgColor
            Cairo.fillPreserve
            cairoSetColor $ func ^. lineColor
            Cairo.setLineWidth $ realToFrac $ func ^. lineWeight
            Cairo.stroke
          return (func ^. plotLabel, func ^. lineColor, y)
    mouse0 <- use $ plotWindow . lastMouseLocation
    lastMouseLocation .= Just mouse1
    case mouse0 of
      Nothing -> return ()
      Just (Mouse _ press0 _mod0 _button0 pt0) -> do
        if press0 && press1 -- if this is a mouse-drag
         then do
          plotWinOrigin += (((pt0 - pt1) ^. pointXY) ^. winToPlotScale plotwin winsize) ^. from pointXY
          (V2 x y) <- use plotWinOrigin
          renderCartesian <$> get <*> pure winsize >>= onCanvas
          onOSBuffer $ screenPrinter $ do
            printerFontStyle . fontForeColor .= black
            printerFontStyle . fontBold .= True
            () <- "Axis Offset:\n"
            printerFontStyle . fontBold .= False
            displayString
              (printf "x = %+.4f\ny = %+.4f\n" (realToFrac x :: Float) (realToFrac y :: Float))
         else clearRegion pt0
    onOSBuffer $ do
      drawGuideLine
      screenPrinter $ do
        textCursor . gridRow    .= 0
        textCursor . gridColumn .= 0
        printerFontStyle . fontForeColor .= black
        displayString
          (printf "mouse: x = %+.4f, y = %+.4f\n"
            (realToFrac x :: Float) (realToFrac y :: Float))
      points <- drawGuidePoints
      screenPrinter $ forM_ points $ \ (label, color, y) -> do
        printerFontStyle . fontForeColor .= color
        displayString (printf "%s = %+.4f\n" label (realToFrac y :: Float))
  resizeCart
{-# SPECIALIZE runCartesian :: GtkGUI (PlotCartesian ProcGenFloat) () #-}
