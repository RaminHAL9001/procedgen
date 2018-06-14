-- | This module provides access to a static stateful variable containing a Gtk window, which is
-- called the "display window." Show content in the window using 'disp' function.
--
-- You can also create an arbitrary application on the fly using 'Happlets.Happlet.makeHapplet' to
-- construct a 'Happlets.Happlet.Happlet' value, and provide this value along with an initializing
-- 'Happlets.GUI.GUI' function to the 'setDisp' function to set an arbitrary model-view-controller.
module ProcGen.GHCI
  ( GHCIDisp(..), setDisp, disp,
  )
  where

import           ProcGen.Music.Synth

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State

import           Happlets.Lib.Gtk
import           Happlets.Provider

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

-- | This class defines the 'disp' function. For data ypes which have instantiated this class, you
-- can evaluate 'disp' to create a window on screen displaying an interactive graphic representing
-- the data type.
class GHCIDisp a where { initDisp :: GtkGUI a (); }
instance GHCIDisp TDView where { initDisp = runTDView; }
instance GHCIDisp FDView where { initDisp = runFDView; }

-- | This function takes any value that instantiates the 'GHCIDisp' class and makes it visible in
-- the diaply window.
disp :: GHCIDisp a => a -> IO ()
disp = makeHapplet >=> flip setDisp (const initDisp)
