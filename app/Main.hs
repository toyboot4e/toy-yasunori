{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed.Mutable qualified as UM
import Raylib.Core qualified as R
import Raylib.Core.Shapes qualified as R
import Raylib.Core.Text qualified as R
import Raylib.Types qualified as R
import Raylib.Util qualified as R
import Raylib.Util.Colors qualified as R
import Yas.Game qualified as Game
import Yas.Prelude

type State = (R.WindowResources, Game.Game RealWorld)

startup :: IO State
startup = do
  window <- R.initWindow w h title
  game <- Game.init
  R.setTargetFPS fps
  return (window, game)
  where
    w = 1280
    h = 720
    title = "Toy Yasunori"
    fps = 60

-- | Cornflower blue.
defaultClearColor :: R.Color
defaultClearColor = R.Color 100 147 237 255

mainLoop :: State -> IO State
mainLoop (!window, !game) = do
  Game.input game
  Game.update game
  R.drawing $ do
    R.clearBackground defaultClearColor
    Game.draw game
  return (window, game)

shouldClose :: State -> IO Bool
shouldClose _ = R.windowShouldClose

teardown :: State -> IO ()
teardown (!window, !_) = R.closeWindow $ Just window

$(R.raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)
