-- | This module provides access to a static stateful variable containing a Gtk window. Set the
-- visibility of the window with 'winvis'. Show content in the window using 'disp'. If the window is
-- closed with the close button, it is necessary to reset the window with 'winreset'.
--
-- Currently, only 'ProcGen.Music.Synth.TDView' is supported by the 'disp' function.
module ProcGen.GHCI
  ( GHCIDisp(..), disp,
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

----------------------------------------------------------------------------------------------------

-- | This class defines the 'disp' function. For data ypes which have instantiated this class, you
-- can evaluate 'disp' to create a window on screen displaying an interactive graphic representing
-- the data type.
class GHCIDisp a where { initDisp :: GtkGUI a (); }
instance GHCIDisp TDView where { initDisp = runTDView; }
instance GHCIDisp FDView where { initDisp = runFDView; }

disp :: GHCIDisp a => a -> IO ()
disp = makeHapplet >=> \ happ -> doWindowAttach gtkHapplet True theWindow happ $ const initDisp
