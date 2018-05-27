-- | This is essentially a test suite which I used to build stand-alone programs when the GHCI
-- version of the program doesn't seem to be working correctly.
module ProcGen.Live where

import           ProcGen
import           Happlets.Lib.Gtk

main :: IO ()
main = do
  fd <- randFDSignalIO 256
  simpleHapplet gtkHapplet
    (do registeredAppName      .= "Procgen-GHCI"
        windowTitleBar         .= "Procgen GHCI"
        decorateWindow         .= True
        deleteWindowOnClose    .= True
        quitOnWindowClose      .= True
        recommendWindowSize    .= (735, 367)
        animationFrameRate     .= 60
        backgroundTransparency .= Just 0.9
    )
    (fdView fd)
    (const runFDView)
