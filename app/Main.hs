{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed.Mutable qualified as UM
import My.Prelude
import Raylib.Core qualified as R
import Raylib.Core.Text qualified as R
import Raylib.Types qualified as R
import Raylib.Util qualified as R
import Raylib.Util.Colors qualified as R

data App = App
  { countA :: !(UM.MVector RealWorld Int)
  }

startup :: IO R.WindowResources
startup = do
  window <- R.initWindow w h title
  R.setTargetFPS fps
  return window
  where
    w = 1280
    h = 720
    title = "Toy Yasunori"
    fps = 60

-- | Cornflower blue.
defaultClearColor :: R.Color
defaultClearColor = R.Color 100 147 237 255

mainLoop :: R.WindowResources -> IO R.WindowResources
mainLoop window = do
  R.drawing $ do
    R.clearBackground defaultClearColor
    R.drawText "You should be yasunori!" 30 40 64 R.lightGray
  return window

shouldClose :: R.WindowResources -> IO Bool
shouldClose _ = R.windowShouldClose

teardown :: R.WindowResources -> IO ()
teardown = R.closeWindow . Just

$(R.raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
