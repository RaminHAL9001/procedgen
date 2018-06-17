-- | Plotting functions.
module ProcGen.Plot where

import           ProcGen.Types

import           Control.Arrow
import           Control.Lens

import           Data.Typeable

import qualified Graphics.Rendering.Cairo as Cairo

import           Happlets.Lib.Gtk

import           Linear.V2

----------------------------------------------------------------------------------------------------

-- | Provides a lens for changing the colour of various things.
class HasLineStyle a where { lineStyle :: Lens' (a num) (LineStyle num); }

data LineStyle num
  = LineStyle
    { theLineColor   :: !PackedRGBA32
    , theLineWeight  :: !num
      -- ^ The weight specified in pixels
    }
  deriving (Eq, Show, Read)

instance HasLineStyle LineStyle where { lineStyle = lens id $ flip const; }

theLineColour :: LineStyle num -> PackedRGBA32
theLineColour = theLineColour

makeLineStyle :: Num num => LineStyle num
makeLineStyle = LineStyle
  { theLineColor  = packRGBA32 0xA0 0xA0 0xA0 0xA0
  , theLineWeight = 2
  }

lineStyleColor :: Lens' (LineStyle num) PackedRGBA32
lineStyleColor = lens theLineColour $ \ a b -> a{ theLineColor = b }

lineStyleColour :: Lens' (LineStyle num) PackedRGBA32
lineStyleColour = lineColor

lineStyleWeight :: Lens' (LineStyle num) num
lineStyleWeight = lens theLineWeight $ \ a b -> a{ theLineWeight = b }

lineColor :: HasLineStyle line => Lens' (line num) PackedRGBA32
lineColor = lineStyle . lineStyleColor

lineColour :: HasLineStyle line => Lens' (line num) PackedRGBA32
lineColour = lineColor

lineWeight :: HasLineStyle line => Lens' (line num) num
lineWeight = lineStyle . lineStyleWeight

----------------------------------------------------------------------------------------------------

data GridLines num
  = GridLines
    { theGridLinesStyle   :: !(LineStyle num)
    , theGridLinesSpacing :: !num
      -- ^ For grid lines, this value is the amount of space between each grid line.
    }
  deriving (Eq, Show, Read)

instance HasLineStyle GridLines where
  lineStyle = lens theGridLinesStyle $ \ a b -> a{ theGridLinesStyle = b }

makeGridLines :: Num num => GridLines num
makeGridLines = GridLines
  { theGridLinesStyle   = makeLineStyle
  , theGridLinesSpacing = 1
  }

gridLinesSpacing :: Lens' (GridLines num) num
gridLinesSpacing = lens theGridLinesSpacing $ \ a b -> a{ theGridLinesSpacing = b }

----------------------------------------------------------------------------------------------------

data PlotAxis num
  = PlotAxis
    { thePlotAxisOffset :: !num
    , thePlotAxisMin    :: !num
    , thePlotAxisMax    :: !num
    , thePlotAxisMajor  :: !(GridLines num)
    , thePlotAxisMinor  :: !(Maybe (GridLines num))
    , thePlotAxisAbove  :: !Bool
      -- ^ True if the grid lines should be drawn on top of the function plot lines.
    }
  deriving (Eq, Show, Read)

makePlotAxis :: Num num => PlotAxis num
makePlotAxis = PlotAxis
  { thePlotAxisOffset = 0
  , thePlotAxisMin    = (-5)
  , thePlotAxisMax    = 5
  , thePlotAxisMajor  = makeGridLines
  , thePlotAxisMinor  = Nothing
  , thePlotAxisAbove  = False
  }

plotAxisMajor :: Lens' (PlotAxis num) (GridLines num)
plotAxisMajor = lens thePlotAxisMajor $ \ a b -> a{ thePlotAxisMajor = b }

plotAxisMinor :: Lens' (PlotAxis num) (Maybe (GridLines num))
plotAxisMinor = lens thePlotAxisMinor $ \ a b -> a{ thePlotAxisMinor = b }

plotAxisAbove :: Lens' (PlotAxis num) Bool
plotAxisAbove = lens thePlotAxisAbove $ \ a b -> a{ thePlotAxisAbove = b }

plotAxisOffset :: Lens' (PlotAxis num) num
plotAxisOffset = lens thePlotAxisOffset $ \ a b -> a{ thePlotAxisOffset = b }

plotAxisMin :: Lens' (PlotAxis num) num
plotAxisMin = lens thePlotAxisMin $ \ a b -> a{ thePlotAxisMin = b }

plotAxisMax :: Lens' (PlotAxis num) num
plotAxisMax = lens thePlotAxisMax $ \ a b -> a{ thePlotAxisMax = b }

----------------------------------------------------------------------------------------------------

-- | Provides the 'grid' lens, which is common to many different plot types, including 'Cartesian'
-- and 'Parametric'.
class HasPlotWindow a where { plotWindow :: Lens' (a num) (PlotWindow num); }

data PlotWindow num
  = PlotWindow
    { theBGColor :: !PackedRGBA32
    , theXAxis   :: !(PlotAxis num)
    , theYAxis   :: !(PlotAxis num)
    }
  deriving (Eq, Show, Read)

instance HasPlotWindow PlotWindow where { plotWindow = lens id $ flip const; }

makePlotWindow :: Num num => PlotWindow num
makePlotWindow = PlotWindow
  { theBGColor = packRGBA32 0xFF 0xFF 0xFF 0xC0
  , theXAxis = makePlotAxis
  , theYAxis = makePlotAxis
  }

bgColor :: Lens' (PlotWindow num) PackedRGBA32
bgColor = lens theBGColor $ \ a b -> a{ theBGColor = b }

plotWindowXAxis :: Lens' (PlotWindow num) (PlotAxis num)
plotWindowXAxis = lens theXAxis $ \ a b -> a{ theXAxis = b }

plotWindowYAxis :: Lens' (PlotWindow num) (PlotAxis num)
plotWindowYAxis = lens theYAxis $ \ a b -> a{ theYAxis = b }

xAxis :: HasPlotWindow win => Lens' (win num) (PlotAxis num)
xAxis = plotWindow . plotWindowXAxis

yAxis :: HasPlotWindow win => Lens' (win num) (PlotAxis num)
yAxis = plotWindow . plotWindowYAxis

----------------------------------------------------------------------------------------------------

-- | Provides a lens for modifying the functions that are plotted.
class HasPlotFunction plot func | plot -> func where
  plotFunctions :: Lens' (plot num) [func num]

data Cartesian num
  = Cartesian
    { theCartStyle    :: !(LineStyle num)
    , theCartFunction :: num -> num
    }

data PlotCartesian num
  = PlotCartesian
    { theCartWindow    :: !(PlotWindow num)
    , theCartFunctions :: [Cartesian num]
    }
  deriving Typeable

instance HasLineStyle Cartesian where
  lineStyle = lens theCartStyle $ \ a b -> a{ theCartStyle = b }

instance HasPlotWindow PlotCartesian where
  plotWindow = lens theCartWindow $ \ a b -> a{ theCartWindow = b }

instance HasPlotFunction PlotCartesian Cartesian where
  plotFunctions = lens theCartFunctions $ \ a b -> a{ theCartFunctions = b }

makeCartesian :: Num num => Cartesian num
makeCartesian = Cartesian
  { theCartStyle    = makeLineStyle{ theLineColor = packRGBA32 0x00 0x00 0xFF 0xFF }
  , theCartFunction = const 0
  }

plotCartesian :: Num num => PlotCartesian num
plotCartesian = PlotCartesian
  { theCartWindow = makePlotWindow
  , theCartFunctions = []
  }

cartFunction :: Lens' (Cartesian num) (num -> num)
cartFunction = lens theCartFunction $ \ a b -> a{ theCartFunction = b }

----------------------------------------------------------------------------------------------------

data Parametric num
  = Parametric
    { theParamStyle  :: !(LineStyle num)
    , theParamTStart :: !num
    , theParamTEnd   :: !num
    , theParamX      :: num -> num
    , theParamY      :: num -> num
    , theParamTStep  :: num -> num
      -- ^ When producing points to place on the plot, an iterator repeatedly increments a "time"
      -- value @t@. For simpler plots, you can increment the value @t@ by a constant value on each
      -- iteration, in which case you would set this function to @'Prelude.const' t@. But for plots
      -- where you may want to vary the amount by which @t@ increments on each iteration, you can
      -- set a more appropriate function here. This allows you to compute a distance between two
      -- parametric points @distance (x(t0), y(t0)) (x(t1), y(t1))@ and solve for @t1@ such that the
      -- distance is always a constant value, creating a smooth parametric curve where all points on
      -- the curve are equadistant from each other.
    }

data PlotParametric num
  = PlotParametric
    { theParamWindow   :: !(PlotWindow num)
    , theParamFunctions :: [Parametric num]
    }
  deriving Typeable

instance HasLineStyle Parametric where
  lineStyle = lens theParamStyle $ \ a b -> a{ theParamStyle = b }

instance HasPlotWindow PlotParametric where
  plotWindow = lens theParamWindow $ \ a b -> a{ theParamWindow = b }

instance HasPlotFunction PlotParametric Parametric where
  plotFunctions = lens theParamFunctions $ \ a b -> a{ theParamFunctions = b }

-- | A default 'Parametric' plotting function. You can set the parameters with various lenses, or
-- using record syntax.
parametric :: Num num => Parametric num
parametric = Parametric
  { theParamStyle  = makeLineStyle{ theLineColor = packRGBA32 0xFF 0x00 0x00 0xFF }
  , theParamTStart = 0
  , theParamTEnd   = 1
  , theParamTStep  = const 1
  , theParamX      = const 0
  , theParamY      = const 0
  }

plotParam :: Num num => PlotParametric num
plotParam = PlotParametric
  { theParamWindow    = makePlotWindow
  , theParamFunctions = [] 
  }

paramTStart :: Lens' (Parametric num) num
paramTStart = lens theParamTStart $ \ a b -> a{ theParamTStart = b }

paramTEnd :: Lens' (Parametric num) num
paramTEnd = lens theParamTEnd $ \ a b -> a{ theParamTEnd = b }

paramTStep :: Lens' (Parametric num) (num -> num)
paramTStep = lens theParamTStep $ \ a b -> a{ theParamTStep = b }

paramX :: Lens' (Parametric num) (num -> num)
paramX = lens theParamX $ \ a b -> a{ theParamX = b }

paramY :: Lens' (Parametric num) (num -> num)
paramY = lens theParamY $ \ a b -> a{ theParamY = b }

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
        (thePlotAxisMax axis, thePlotAxisMin axis)
  let plotWin = hi - lo :: Double
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

drawCart :: (Real num, Fractional num) => PlotCartesian num -> PixSize -> GtkRedraw ()
drawCart plot size@(V2 (SampCoord w) (SampCoord h)) = cairoRender $ do
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
  when (xwin /= 0 && ywin /= 0) $ forM_ (plot ^. plotFunctions) $ \ cart -> do
    let f = ytrans . (cart ^. cartFunction) . xtrans
    let xstep = realToFrac w / xwin
    setLineStyle (cart ^. lineStyle)
    let x0 = if xstep < 0 then realToFrac w else 0
    let y0 = f $ realToFrac x0
    Cairo.moveTo x0 (realToFrac y0)
    forM_ (if xstep < 0 then [w, w-1 .. 0] else [0 .. w]) $ \ x -> do
      Cairo.lineTo (realToFrac x) (realToFrac $ f $ realToFrac x)
    Cairo.stroke
  drawAxis when

resizeCart :: (Real num, Fractional num) => GtkGUI (PlotCartesian num) ()
resizeCart = getModel >>= void . onCanvas . drawCart

runCartesian :: (Real num, Fractional num) => GtkGUI (PlotCartesian num) ()
runCartesian = do
  resizeEvents $ const resizeCart
  resizeCart

----------------------------------------------------------------------------------------------------

-- | Example 'PlotCartesian' function which plots a Gaussian curve.
example :: PlotCartesian ProcGenFloat
example = plotCartesian &~ do
  plotFunctions .=
    [ makeCartesian &~ do
        cartFunction .= sigmoid TimeWindow{ timeStart = (-1), timeEnd = 1 } . negate
        lineColor    .= packRGBA32 0x00 0x00 0xFF 0xFF
        lineWeight   .= 3.0
    ]
  let axis = makePlotAxis &~ do
        plotAxisOffset .= 0.0
        plotAxisMin    .= (-1.0)
        plotAxisMax    .= 1.0
        plotAxisMajor  %= flip (&~)
          (do gridLinesSpacing .= 0.5
              lineColor        .= packRGBA32 0x40 0x40 0x40 0xA0
              lineWeight       .= 2.0
          )
        plotAxisMinor  .= Just
          ( makeGridLines &~ do
              gridLinesSpacing .= 0.1
              lineColor        .= packRGBA32 0x80 0x80 0x80 0x80
              lineWeight       .= 1.0
          )
  plotWindow %= flip (&~)
    (do xAxis .= axis
        yAxis .= (plotAxisMin .~ 0.0) axis
    )
