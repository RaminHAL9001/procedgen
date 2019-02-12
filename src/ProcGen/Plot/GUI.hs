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

import           Text.Printf

----------------------------------------------------------------------------------------------------

clearWithBGColor :: Color -> Cairo.Render ()
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
{-# SPECIALIZE setLineStyle :: LineStyle ProcGenFloat -> Cairo.Render () #-}

drawPlotWindow :: RealFrac num => PlotWindow num -> PixSize -> Cairo.Render ()
drawPlotWindow plotwin size@(V2 w h) = do
  let lim = canonicalRect2D $ winToPlotRect plotwin size
  let pp2wpX = fromPlotToWinX plotwin size
  let pp2wpY = fromPlotToWinY plotwin size
  let steps pp2wp gap lo hi =
        let s = lo / gap
            i = realToFrac (floor s :: Int) * gap
        in  fmap pp2wp $ takeWhile (<= hi) $ iterate (gap +) i
  let vertical   ymax x = do
        Cairo.moveTo (realToFrac x + 0.5) (0.0)
        Cairo.lineTo (realToFrac x + 0.5) (realToFrac ymax)
        Cairo.stroke
  let horizontal xmax y = do
        Cairo.moveTo (0.0)             (realToFrac y + 0.5)
        Cairo.lineTo (realToFrac xmax) (realToFrac y + 0.5)
        Cairo.stroke
  let drawLines paint wmax pp2wp dim point = do
        let axis = plotwin ^. dim
        let run majmin = case axis ^. majmin of
              Nothing  -> return ()
              Just gap -> do
                setLineStyle $ gap ^. lineStyle
                mapM_ (paint wmax) $ steps pp2wp
                  (gap ^. gridLinesSpacing) (lim ^. rect2DHead . point) (lim ^. rect2DTail . point)
        run (axisMajor . re _Just) >> run axisMinor
        case axis ^. axisDrawOrigin  of
          Nothing     -> return ()
          Just origin -> setLineStyle origin >> paint wmax (pp2wp 0)
  drawLines  vertical   h pp2wpX xDimension pointX
  drawLines  horizontal w pp2wpY yDimension pointY
{-# SPECIALIZE drawPlotWindow :: PlotWindow ProcGenFloat -> PixSize -> Cairo.Render () #-}

renderCartesian :: forall num . RealFrac num => PlotCartesian num -> PixSize -> CairoRender ()
renderCartesian plot size@(V2 w _h) = cairoRender $ do
  let plotwin = plot ^. plotWindow
  let drawAxis isAbove = do
        isAbove (plot ^. xDimension . axisAbove) $ drawPlotWindow plotwin size
        isAbove (plot ^. yDimension . axisAbove) $ drawPlotWindow plotwin size
  clearWithBGColor (plotwin ^. bgColor)
  drawAxis unless
  forM_ (reverse $ plot ^. plotFunctionList) $ \ cart -> do
    let f   = theCartIterator cart
    let win = uncurry TimeWindow $ plotwin ^. dimX ^. axisBounds
    case f (fromIntegral $ div w 2) win :: [(num, num)] of
      []        -> return ()
      p0:points -> do
        cairoSetColor $ cart ^. lineColor
        Cairo.setLineWidth $ realToFrac $ cart ^. lineWeight
        let pp2wp = from (winToPlotPoint plotwin size) :: Iso' (num, num) (SampCoord, SampCoord)
        let r2f2 = realToFrac *** realToFrac :: (SampCoord, SampCoord) -> (Double, Double)
        uncurry Cairo.moveTo $ r2f2 $ p0 ^. pp2wp
        forM_ points $ uncurry Cairo.lineTo . r2f2 . (^. pp2wp)
        Cairo.stroke
  drawAxis when
{-# SPECIALIZE renderCartesian :: PlotCartesian ProcGenFloat -> PixSize -> CairoRender () #-}

resizeCart :: RealFrac num => OldPixSize -> NewPixSize -> GtkGUI (PlotCartesian num) ()
resizeCart _ siz = renderCartesian <$> getModel <*> pure siz >>= onCanvas
{-# SPECIALIZE resizeCart :: OldPixSize -> NewPixSize -> GtkGUI (PlotCartesian ProcGenFloat) () #-}

----------------------------------------------------------------------------------------------------

runCartesian :: forall num . RealFrac num => GtkGUI (PlotCartesian num) ()
runCartesian = do
  resizeEvents CopyCanvasMode resizeCart
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
    let (x, y) = pt1 ^. pointXY ^. winToPlotPoint plotwin winsize :: (num, num)
    let drawGuideLine = unless (null funcList) $
          drawLine (negColor $ plotwin ^. bgColor) 1.0 $ line2D &~ do
            let x = realToFrac x1 + 0.5
            line2DHead .= V2 x 0
            line2DTail .= V2 x (realToFrac winH)
    let drawGuidePoints = forM (reverse funcList) $ \ func -> do
          let y = theCartFunction (func :: Cartesian num) x
              -- HERE ^ is where the plot function is evaluaed
          let cairoCoord = (+ 0.5) . realToFrac :: SampCoord -> Double
          let (xp, yp) = cairoCoord *** cairoCoord $ (x, y) ^. from (winToPlotPoint plotwin winsize)
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
          cancelIfBusy
          (V2 x y) <- use plotWinOrigin
          renderCartesian <$> get <*> pure winsize >>= onCanvas
          void $ onOSBuffer $ screenPrinter $ do
            fontStyle . fontForeColor .= black
            fontStyle . fontBold .= True
            () <- "Axis Offset:\n"
            fontStyle . fontBold .= False
            displayString
              (printf "x = %+.4f\ny = %+.4f\n" (realToFrac x :: Float) (realToFrac y :: Float))
         else clearRegion pt0
    onOSBuffer $ do
      drawGuideLine
      screenPrinter $ do
        textCursor . gridRow    .= 0
        textCursor . gridColumn .= 0
        fontStyle . fontForeColor .= black
        displayString
          (printf "mouse: x = %+.4f, y = %+.4f\n"
            (realToFrac x :: Float) (realToFrac y :: Float))
      points <- drawGuidePoints
      screenPrinter $ forM_ points $ \ (label, color, y) -> do
        fontStyle . fontForeColor .= color
        displayString (printf "%s = %+.4f\n" label (realToFrac y :: Float))
{-# SPECIALIZE runCartesian :: GtkGUI (PlotCartesian ProcGenFloat) () #-}
