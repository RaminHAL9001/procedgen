-- | Plotting functions. This module only contains the data types and lenses. More useful functions
-- are provided in the "ProcGen.PlotGUI" module which also re-exports this module, so usually you
-- want to import "ProcGen.PlotGUI" instead of this module alone, unless you do not intend to use
-- the associated GUI functions.
module ProcGen.Plot where

import           ProcGen.Types

import           Control.Lens

import qualified Data.Text       as Strict
import           Data.Typeable

import           Happlets.Lib.Gtk

import           Linear.V2

----------------------------------------------------------------------------------------------------

class HasPlotLabel func where { plotLabel :: Lens' (func num) Strict.Text }

----------------------------------------------------------------------------------------------------

-- | Provides a lens for changing the colour of various things.
class HasLineStyle a where { lineStyle :: Lens' (a num) (LineStyle num); }

data LineStyle num
  = LineStyle
    { theLineColor  :: !Color
    , theLineWeight :: !num
      -- ^ The weight specified in pixels
    }
  deriving (Eq, Show, Read)

instance HasLineStyle LineStyle where { lineStyle = lens id $ flip const; }

theLineColour :: LineStyle num -> Color
theLineColour = theLineColor

makeLineStyle :: Num num => LineStyle num
makeLineStyle = LineStyle
  { theLineColor  = packRGBA32 0xA0 0xA0 0xA0 0xA0
  , theLineWeight = 2
  }

lineColor :: HasLineStyle line => Lens' (line num) Color
lineColor = lineStyle . lens theLineColor (\ a b -> a{ theLineColor = b })

lineColour :: HasLineStyle line => Lens' (line num) Color
lineColour = lineColor

lineWeight :: HasLineStyle line => Lens' (line num) num
lineWeight = lineStyle . lens theLineWeight (\ a b -> a{ theLineWeight = b })

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
    { theBGColor           :: !Color
    , theXAxis             :: !(PlotAxis num)
    , theYAxis             :: !(PlotAxis num)
    , theLastMouseLocation :: Maybe Mouse
    }
  deriving (Eq, Show)

instance HasPlotWindow PlotWindow where { plotWindow = lens id $ flip const; }

makePlotWindow :: Num num => PlotWindow num
makePlotWindow = PlotWindow
  { theBGColor           = packRGBA32 0xFF 0xFF 0xFF 0xC0
  , theXAxis             = makePlotAxis
  , theYAxis             = makePlotAxis
  , theLastMouseLocation = Nothing
  }

bgColor :: Lens' (PlotWindow num) Color
bgColor = lens theBGColor $ \ a b -> a{ theBGColor = b }

lastMouseLocation :: HasPlotWindow win => Lens' (win num) (Maybe Mouse)
lastMouseLocation = plotWindow .
  lens theLastMouseLocation (\ a b -> a{ theLastMouseLocation = b })

xAxis :: HasPlotWindow win => Lens' (win num) (PlotAxis num)
xAxis = plotWindow . lens theXAxis (\ a b -> a{ theXAxis = b })

yAxis :: HasPlotWindow win => Lens' (win num) (PlotAxis num)
yAxis = plotWindow . lens theYAxis (\ a b -> a{ theYAxis = b })

plotOrigin :: HasPlotWindow win => Lens' (win num) (V2 num)
plotOrigin = lens (\ win -> V2 (win ^. xAxis . plotAxisOffset) (win ^. yAxis . plotAxisOffset))
  (\ win (V2 x y) -> win &~ do
      xAxis . plotAxisOffset .= x
      yAxis . plotAxisOffset .= y
  )

-- | Convert a window point to a plot point given the window size as a 'Happlets.SampCoord.PixSize'
-- type, and the 'ProcGen.Plot.PlotWindow' information. Convert in the other direction (from plot
-- points to window points) using @('Control.Lens.from' 'winToPointPlot')@.
--
-- This function is used as an intermediate computational step, and so is designed to be a lazy as
-- possible. This is the reason why tuples are used as inputs and outputs, rather than taking a
-- 'Linear.V2.V2' type or 'Happlets.Types2D.Point2D' type. But you can be sure that if you only use
-- the 'Prelude.fst' value of the result, the 'Prelude.snd' value will not be computed, so it ends
-- up being more efficient.
-- 
-- This is a lossy isomorphism, meaning the @num@ type you choose (usually 'Prelude.Float' or
-- 'Prelude.Double') performs some approximation such that when you evaluate an input to produce an
-- output, evaluating this function in the inverse direction on the output may not produce the same
-- input value you started, at least not for the most extreme values.
winToPlotPoint
  :: RealFrac num
  => PlotWindow num -> PixSize -> Iso' (SampCoord, SampCoord) (num, num)
winToPlotPoint plotwin winsize = winToPlotScale plotwin winsize . winToPlotOffset plotwin

-- | Convert from a 'Happlets.Draw.SampCoord.SampCoord' in the X-axis to a plot local coordinate in
-- the X axis.
fromWinToPlotX :: RealFrac num => PlotWindow num -> PixSize -> SampCoord -> num
fromWinToPlotX plotwin winsize coord =
  fst $ (coord, error "fromWinToPlotX tried to use Y") ^. winToPlotPoint plotwin winsize

-- | Convert from a 'Happlets.Draw.SampCoord.SampCoord' in the Y-axis to a plot local coordinate in
-- the Y-axis.
fromWinToPlotY :: RealFrac num => PlotWindow num -> PixSize -> SampCoord -> num
fromWinToPlotY plotwin winsize coord =
  snd $ (error "fromWinToPlotY tried to use X", coord) ^. winToPlotPoint plotwin winsize

-- | Convert from a plot local coordinate in the X-axis to a 'Happlets.Draw.SampCoord.SampCoord'
-- coordinate in the X-axis.
fromPlotToWinX :: RealFrac num => PlotWindow num -> PixSize -> num -> SampCoord
fromPlotToWinX plotwin winsize coord =
  fst $ (coord, error "fromPlotToWinX tried to use Y") ^. from (winToPlotPoint plotwin winsize)

-- | Convert from a plot local coordinate in the Y-axis to a 'Happlets.Draw.SampCoord.SampCoord'
-- coordinate in the Y-axis.
fromPlotToWinY :: RealFrac num => PlotWindow num -> PixSize -> num -> SampCoord
fromPlotToWinY plotwin winsize coord =
  snd $ (error "fromPlotToWinY tried to use X", coord) ^. from (winToPlotPoint plotwin winsize)

-- | Similar to 'winPointToPlotPoint' but only scales the point, it does not offset the point. This
-- is useful when converting changes in position given in window coordinates to changes in position
-- given in plot coordinates. 
winToPlotScale
  :: RealFrac num
  => PlotWindow num -> PixSize -> Iso' (SampCoord, SampCoord) (num, num)
winToPlotScale plotwin (V2 winW winH) =
  let xaxis = plotwin ^. xAxis
      yaxis = plotwin ^. yAxis
      (xlo, xhi) = (xaxis ^. plotAxisMin, xaxis ^. plotAxisMax)
      (ylo, yhi) = (yaxis ^. plotAxisMin, yaxis ^. plotAxisMax)
      xwin = xhi - xlo
      ywin = yhi - ylo
      xscale = xwin / realToFrac winW
      yscale = ywin / realToFrac winH
  in  iso (\ (x, y) -> (realToFrac x * xscale, negate $ realToFrac y * yscale))
          (\ (x, y) -> (round    $ x / xscale, negate $ round    $ y / yscale))

-- | Offset the value of the result of a 'winToPlotScale' conversion by the 'plotOrigin'.
winToPlotOffset :: RealFrac num => PlotWindow num -> Iso' (num, num) (num, num)
winToPlotOffset plotwin = let (dx, dy) = plotwin ^. plotOrigin . pointXY in
  iso (\ (x, y) -> (x + dx - 1, y + dy + 1)) (\ (x, y) -> (x - dx + 1, y - dy - 1))
  -- NOTE: adding or subtracting 1 from the offsets accounts for the width of the window after
  -- scaling.

-- | Create a 'Happlets.Types2D.Rect2D' that demarks the boundary (in plot units) of the view screen
-- window.
winToPlotRect :: RealFrac num => PlotWindow num -> PixSize -> Rect2D num
winToPlotRect plotwin size@(V2 w h) = rect2D &~ do
  rect2DHead .= (0, 0) ^. winToPlotPoint plotwin size . from pointXY
  rect2DTail .= (w, h) ^. winToPlotPoint plotwin size . from pointXY

----------------------------------------------------------------------------------------------------

-- | Provides a lens for modifying the functions that are plotted.
class HasPlotFunction plot func | plot -> func where
  plotFunctionList :: Lens' (plot num) [func num]

class HasDefaultPlot func where { defaultPlot :: Num num => func num; }

data Cartesian num
  = Cartesian
    { theCartLabel    :: !Strict.Text
    , theCartStyle    :: !(LineStyle num)
    , theCartFunction :: num -> num
    }

data PlotCartesian num
  = PlotCartesian
    { theCartWindow       :: !(PlotWindow num)
    , theCartFunctionList :: [Cartesian num]
    }
  deriving Typeable

instance HasPlotLabel Cartesian where
  plotLabel = lens theCartLabel $ \ a b -> a{ theCartLabel = b }

instance HasLineStyle Cartesian where
  lineStyle = lens theCartStyle $ \ a b -> a{ theCartStyle = b }

instance HasPlotWindow PlotCartesian where
  plotWindow = lens theCartWindow $ \ a b -> a{ theCartWindow = b }

instance HasPlotFunction PlotCartesian Cartesian where
  plotFunctionList = lens theCartFunctionList $ \ a b -> a{ theCartFunctionList = b }

instance HasDefaultPlot Cartesian where { defaultPlot = makeCartesian; }

makeCartesian :: Num num => Cartesian num
makeCartesian = Cartesian
  { theCartLabel    = ""
  , theCartStyle    = makeLineStyle{ theLineColor = packRGBA32 0x00 0x00 0xFF 0xFF }
  , theCartFunction = const 0
  }

cartFunction :: Lens' (Cartesian num) (num -> num)
cartFunction = lens theCartFunction $ \ a b -> a{ theCartFunction = b }

plotCartesian :: Num num => PlotCartesian num
plotCartesian = PlotCartesian
  { theCartWindow       = makePlotWindow
  , theCartFunctionList = []
  }

----------------------------------------------------------------------------------------------------

data Parametric num
  = Parametric
    { theParamLabel  :: !Strict.Text
    , theParamStyle  :: !(LineStyle num)
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
    { theParamWindow       :: !(PlotWindow num)
    , theParamFunctionList :: [Parametric num]
    }
  deriving Typeable

instance HasPlotLabel Parametric where
  plotLabel = lens theParamLabel $ \ a b -> a{ theParamLabel = b }

instance HasLineStyle Parametric where
  lineStyle = lens theParamStyle $ \ a b -> a{ theParamStyle = b }

instance HasPlotWindow PlotParametric where
  plotWindow = lens theParamWindow $ \ a b -> a{ theParamWindow = b }

instance HasPlotFunction PlotParametric Parametric where
  plotFunctionList = lens theParamFunctionList $ \ a b -> a{ theParamFunctionList = b }

instance HasDefaultPlot Parametric where { defaultPlot = parametric; }

-- | A default 'Parametric' plotting function. You can set the parameters with various lenses, or
-- using record syntax.
parametric :: Num num => Parametric num
parametric = Parametric
  { theParamLabel  = ""
  , theParamStyle  = makeLineStyle{ theLineColor = packRGBA32 0xFF 0x00 0x00 0xFF }
  , theParamTStart = 0
  , theParamTEnd   = 1
  , theParamTStep  = const 1
  , theParamX      = const 0
  , theParamY      = const 0
  }

plotParam :: Num num => PlotParametric num
plotParam = PlotParametric
  { theParamWindow       = makePlotWindow
  , theParamFunctionList = [] 
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
