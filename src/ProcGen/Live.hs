-- | This is essentially a test suite which I used to build stand-alone programs when the GHCI
-- version of the program doesn't seem to be working correctly.
module ProcGen.Live where

import           ProcGen

main :: IO ()
main = do
  --fd <- randFDSignalIO 256
  happlet gtkHapplet $ do
    registeredAppName      .= "Procgen-GHCI"
    windowTitleBar         .= "Procgen GHCI"
    decorateWindow         .= True
    deleteWindowOnClose    .= True
    quitOnWindowClose      .= True
    recommendWindowSize    .= (735, 367)
    animationFrameRate     .= 60
    backgroundTransparency .= Just 0.9
    win <- newWindow
    cp1 <- newHapplet example
    attachWindow True win cp1 $ const $ runCartesian
  --msgs <- onHapplet cp1 $ \ cp1 -> return (cp1 ^. cartLog, cp1 & cartLog .~ "")
  --Lazy.putStr (fst msgs)
