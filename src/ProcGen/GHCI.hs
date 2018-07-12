-- | This module provides access to a static stateful variable containing a Gtk window, which is
-- called the "display window." Show content in the window using 'disp' function.
--
-- You can also create an arbitrary application on the fly using 'Happlets.Happlet.makeHapplet' to
-- construct a 'Happlets.Happlet.Happlet' value, and provide this value along with an initializing
-- 'Happlets.GUI.GUI' function to the 'setDisp' function to set an arbitrary model-view-controller.
module ProcGen.GHCI
  ( GHCIDisp(..), setDisp, disp, chapp,
    -- * Cartesian Plotting
    newCartWin, cart, exampleCart,
    -- * Parametric Plotting
    newPlotWin, newParamWin, param,
    -- * Plot Lists
    -- $PlotLists
    modifyLayers, copyLayers, getLayers, deleteLayers, partLayers, rakeUp, rakeDown,
    addLayer, onLayer, onLayerGroup, onTopLayer,
    -- * Functions with polymorphic types.
    liveUpdate, currentHapplet,
    -- * Working with persistent values in the GHCI process.
    module ProcGen.PlotGUI,
    module ProcGen.ReaderLogic,
    module Happlets.Lib.Gtk,
  )
  where

import           ProcGen.Music.Synth
import           ProcGen.PlotGUI
import           ProcGen.ReaderLogic
import           ProcGen.Types

import           Control.Arrow
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State

import           Happlets.Lib.Gtk
import           Happlets.Provider

import           Data.Dynamic
import           Data.List      (partition)
import           Data.Typeable

import           System.IO.Unsafe

----------------------------------------------------------------------------------------------------

ghciGtkConfig :: Config
ghciGtkConfig = flip execState (defaultConfig gtkHapplet) $ do
  registeredAppName      .= "Procgen-GHCI"
  windowTitleBar         .= "Procgen GHCI"
  decorateWindow         .= True
  deleteWindowOnClose    .= False
  quitOnWindowClose      .= False
  recommendWindowSize    .= (735, 367)
  animationFrameRate     .= 60
  backgroundTransparency .= Just 0.9

theWindow :: GtkWindow
theWindow = unsafePerformIO $ do
  doInitializeGUI gtkHapplet
  window <- doWindowNew gtkHapplet ghciGtkConfig
  forkOS $ gtkLaunchEventLoop ghciGtkConfig
  return window
{-# NOINLINE theWindow #-}

-- | Use this function to switch the Happlet that is currently being displayed in the display
-- window.
setDisp :: Happlet model -> (PixSize -> GtkGUI model ()) -> IO ()
setDisp = doWindowAttach gtkHapplet True theWindow

----------------------------------------------------------------------------------------------------

-- | This class defines the 'disp' function. For data types which have instantiated this class, you
-- can evaluate 'disp' to create a window on screen displaying an interactive graphic representing
-- the data type.
class GHCIDisp model where { initDisp :: GtkGUI model (); }
instance GHCIDisp TDView where { initDisp = runTDView; }
instance GHCIDisp FDView where { initDisp = runFDView; }
instance RealFrac num => GHCIDisp (PlotCartesian num) where
  initDisp = runCartesian

-- | This function takes any value that instantiates the 'GHCIDisp' class and makes it visible in
-- the diaply window.
disp :: GHCIDisp model => model -> IO ()
disp = makeHapplet >=> flip setDisp (const initDisp)

----------------------------------------------------------------------------------------------------

data CurrentHapplet
  = CurrentHapplet
    { dynamicHapplet  :: Dynamic
    , switchToHapplet :: IO ()
    }
  deriving Typeable

theCurrentHapplet :: MVar CurrentHapplet
theCurrentHapplet = unsafePerformIO $ newMVar $ CurrentHapplet
   { dynamicHapplet  = toDyn ()
   , switchToHapplet = error "Must first call 'chapp'"
   }

-- | Change the Happlet that will be displayed in the view window for live coding, passing an
-- initializing 'Happlets.GUI.GUI' function to install the event handlers. This function operates
-- just like 'setDisp' except it also keeps a copy of the 'Happlets.Happlet.Happlet' reference in a
-- global static variable so that it the 'Happlets.Happlet.Happlet' @model can be modified with the
-- 'live' function.
currentHapplet :: Typeable model => Happlet model -> GtkGUI model () -> IO ()
currentHapplet happ initDisp = do
  let switch = setDisp happ $ const initDisp
  modifyMVar_ theCurrentHapplet $ const $ return $ CurrentHapplet
    { dynamicHapplet  = toDyn happ
    , switchToHapplet = switch
    }
  switch

-- | "Change Happlet": this function is just ilke 'currentHapplet', this function changes the
-- Happlet that will be displayed in the view window for live coding. However 'chapp' uses the
-- 'GHCIDisp' default function as the initializing function.
chapp :: (GHCIDisp model, Typeable model) => Happlet model -> IO ()
chapp happ = currentHapplet happ initDisp

_live :: Typeable model => model -> GtkGUI model a -> IO a
_live ~witness f = do
  mvar <- newEmptyMVar
  withMVar theCurrentHapplet $ dynamicHapplet >>> \ dyn ->
    let happ = fromDyn dyn $ error $ "live: cannot evaluate GUI function of type "++
          show (typeOf witness)++", current Happlet set by 'chapp' is of type "++
          show (dynTypeRep dyn)
    in  fmap fst $ onHapplet happ $
          evalGUI (f >>= liftIO . putMVar mvar) happ theWindow >=> \ st ->
          takeMVar mvar >>= \ a ->
          return (a, theGUIModel st)

-- | Evaluate a 'Happlets.Lib.Gtk.GtkGUI' function on the 'Happlets.Happlet.Happlet' most recently
-- selected by the 'chapp' function. This function alone isn't terribly useful when your 'GtkGUI'
-- @model@ type is polymophic, you must explicitly specify the exact type of 'GtkGUI' function you
-- want to evaluate. However this function can be used to delcare type-specific updates on the
-- current Happlet. So you can use 'liveUpdate' to declare other "live" functions with more specific
-- types, like 'cart' and 'param' are specifically-typed versions of this 'liveUpdate' function.
liveUpdate :: Typeable model => GtkGUI model a -> IO a
liveUpdate = _live (error "'ProcGen.GHCI.live' function attempted to evaluate a witness value")

newPlotWin :: HasPlotWindow plot => plot ProcGenFloat -> IO (Happlet (plot ProcGenFloat))
newPlotWin makeWin = makeHapplet $ makeWin &~ do
  let axis = makePlotAxis &~ do
        plotAxisOffset .= (0.0 :: ProcGenFloat)
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
        yAxis .= axis
    )

----------------------------------------------------------------------------------------------------

-- $PlotLists
--
-- These functions can modify the 'ProcGen.Plot.plotFunctionList' of some plot window, either a
-- 'ProcGen.Plot.Cartesian' or 'ProcGen.Plot.Parametric' plot. These function types are polymorphic
-- over the @model@, evaluate these functions using 'cart' or 'param' to bind the @model@ type.

-- | Copy the plot list of the current plot window, filter and return the copy.
copyLayers
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => ([func num] -> [func num]) -- ^ A function to filter the @plot@s to be returned.
  -> GtkGUI model [func num]
copyLayers filter = modifyLayers [] $ state . const . (id &&& filter)

-- | Copy and return plot functions that match the given predicate.
getLayers
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => PredicateOn (func num)
  -> GtkGUI model [func num]
getLayers = copyLayers . filter . runReader

partLayers
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => (([func num], [func num]) -> ([func num], [func num]))
     -- ^ A function to re-order the functions filtered by the partitioning predicate.
  -> PredicateOn (func num)
  -> GtkGUI model [func num]
partLayers reorder select =
  modifyLayers [] $ state . const . reorder . partition (runReader select)

-- | Select all plots that match the given predicate and move them to the "top" of the drawing
-- order. "Top" means drawn on top of all others so it obscures plots drawn below it.
rakeUp
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => PredicateOn (func num)
  -> GtkGUI model [func num]
rakeUp = partLayers (\ (yes, no) -> (yes ++ no, yes))

-- | Select all plots that match the given predicate and move them to the "bottom" of the drawing
-- order. "Bottom" means drawn below all others so it is obscured by every plot drawn above it.
rakeDown
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => PredicateOn (func num)
  -> GtkGUI model [func num]
rakeDown = partLayers (\ (yes, no) -> (no ++ yes, yes))

-- | Insert a plot function at the top of the stack, use the state lens update functions to define the plot.
addLayer
  :: (HasPlotWindow plot, HasPlotFunction plot func, HasDefaultPlot func,
      Typeable model, Num num, model ~ plot num)
  => State (func num) ()
  -> GtkGUI model ()
addLayer select = modifyLayers () $ return . ((execState select defaultPlot) :)

-- | Evaluate an updating function on all plots matching a given predicate. Returns a copy of the
-- updated plots.
onLayer
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => PredicateOn (func num)
  -> State (func num) ()
  -> GtkGUI model [func num]
onLayer select f = modifyLayers [] $ state . const .
  (fmap (\ layer -> if runReader select layer then execState f layer else layer) &&&
   filter (runReader select)
  )

onLayerGroup
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => ([func num] -> ([func num], [func num] -> [func num]))
     -- ^ A function that takes a section of elements from a list, then returns the selection along
     -- with a function on how to re-constitute the selection with the list from which it was
     -- pulled.
  -> State (func num) ()
  -> GtkGUI model [func num]
onLayerGroup select f = modifyLayers [] $ state . const .
  (\ (old, regroup) -> let new = execState f <$> old in (regroup new, new)) . select

-- | Operate on the top-most plot.
onTopLayer
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => State (func num) ()
  -> GtkGUI model [func num]
onTopLayer = onLayerGroup $ splitAt 1 >>> \ (lo, hi) -> (lo, (++ hi))

-- | Remove plots that match the filter. Returns the deleted plots.
deleteLayers
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => PredicateOn (func num)
  -> GtkGUI model [func num]
deleteLayers select = modifyLayers [] $ state . const . partition (not . runReader select)

-- | Operate on the plot list of the current plot window, with a fold value of type @st@.
modifyLayers
  :: (HasPlotWindow plot, HasPlotFunction plot func, Typeable model, model ~ plot num)
  => st
  -> ([func num] -> State st [func num])
      -- ^ A function to fold over the @plot@s, return @plot@s to replace it with.
  -> GtkGUI model st
modifyLayers st f = do
  list <- use plotFunctionList
  (list, st) <- pure $ runState (f list) st
  plotFunctionList .= list
  return st

----------------------------------------------------------------------------------------------------

-- | Create a Happlet for displaying cartesian plots, with a default major and minor grid lines. Use
-- 'chapp' to change to the 'Happlets.Happlet.Happlet' returned by this function. Then use 'cart' to
-- make modifications to it.
newCartWin :: IO (Happlet (PlotCartesian ProcGenFloat))
newCartWin = newPlotWin plotCartesian

-- | If you have 'chapp'-ed to a 'PlotCartesian' view, you can use 'cart' to live-update the view.
cart :: GtkGUI (PlotCartesian ProcGenFloat) a -> IO a
cart = liveUpdate

-- | This is an example cartesian plot. Use this command to view it:
--
-- @
-- 'ProcGen.Plot.plotFunctions' 'Control.Lens..=' ['exampleCart']
-- @
exampleCart :: Cartesian ProcGenFloat
exampleCart = makeCartesian &~ do
  cartFunction .= sigmoid TimeWindow{ timeStart = (-1), timeEnd = 1 } . negate
  lineColor    .= packRGBA32 0x00 0x00 0xFF 0xFF
  lineWeight   .= 3.0

----------------------------------------------------------------------------------------------------

-- | Create a Happlet for displaying cartesian plots, with a default major and minor grid lines. Use
-- 'chapp' to change to the 'Happlets.Happlet.Happlet' returned by this function. Then use 'cart' to
-- make modifications to it.
newParamWin :: IO (Happlet (PlotParametric ProcGenFloat))
newParamWin = newPlotWin plotParam

-- | If you have 'chapp'-ed to a 'PlotParametric' view, you can use 'param' to live-update the view.
param :: GtkGUI (PlotParametric ProcGenFloat) a -> IO a
param = liveUpdate
