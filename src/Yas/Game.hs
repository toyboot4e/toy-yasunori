module Yas.Game (Game (..), init, input, update, draw) where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Ix
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Raylib.Core qualified as R
import Raylib.Core.Shapes qualified as R
import Raylib.Core.Text qualified as R
import Raylib.Types qualified as R
import Raylib.Util qualified as R
import Raylib.Util.Colors qualified as R
import Yas.Entity.RectF qualified as RF
import Yas.Entity.Vec2F qualified as VF
import Yas.Prelude
import Prelude hiding (init)

data Game s = Game
  { nG :: {-# UNPACK #-} !Int,
    isStartedG :: !(UM.MVector s Bool),
    isDeadG :: !(UM.MVector s Bool),
    rectsG :: !(UM.MVector s RF.RectF),
    speedsG :: !(UM.MVector s VF.Vec2F),
    aliveG :: !(UM.MVector s Bool)
  }

data FrozenGame = FrozenGame
  { isStartedFG :: !Bool,
    isDeadFG :: !Bool,
    rectsFG :: !(U.Vector RF.RectF),
    speedsFG :: !(U.Vector VF.Vec2F),
    aliveFG :: !(U.Vector Bool)
  }
  deriving (Show, Eq)

-- | The player index.
yasunori :: Int
yasunori = 0

-- | The ball index.
ball :: Int
ball = 1

isErasableBlock :: Int -> Bool
isErasableBlock = (>= 6)

boundsL :: Double
boundsL = 0

boundsR :: Double
boundsR = 1280

boundsU :: Double
boundsU = 0

boundsD :: Double
boundsD = 720

boundsW :: Double
boundsW = 1280

boundsH :: Double
boundsH = 720

initialBallSpeed :: Double
initialBallSpeed = 6

barSpeed :: Double
barSpeed = 6

-- | \(O(1)\)
freeze :: (PrimMonad m) => Game (PrimState m) -> m FrozenGame
freeze Game {..} = do
  isStartedFG <- U.head <$> U.unsafeFreeze isStartedG
  isDeadFG <- U.head <$> U.unsafeFreeze isDeadG
  rectsFG <- U.unsafeFreeze rectsG
  speedsFG <- U.unsafeFreeze speedsG
  aliveFG <- U.unsafeFreeze aliveG
  return FrozenGame {..}

-- | \(O(n)\)
init :: (PrimMonad m) => m (Game (PrimState m))
init = do
  isStartedG <- UM.replicate 1 False
  isDeadG <- UM.replicate 1 False

  let unit = 24
  rectsG <- do
    let barW = 120
    let barH = unit
    let y0 = 4
    U.unsafeThaw . U.fromList $
      [ -- bar named yasunori
        RF.RectF (boundsW / 2 - barW / 2) (boundsH - barH - 20) barW barH,
        -- ball
        RF.RectF (boundsW / 2 - unit / 2) (boundsH - barH - 20 - unit) unit unit,
        -- bounds
        RF.RectF 0 (-1) boundsW 1,
        RF.RectF 0 boundsH boundsW 1,
        RF.RectF (-1) 0 1 boundsH,
        RF.RectF boundsW 0 1 boundsH,
        -- y
        RF.RectF (unit * 10) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 12) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 10) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 12) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 10) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 11) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 12) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 12) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 12) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 11) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 10) (unit * (y0 + 4)) unit unit,
        -- A
        RF.RectF (unit * 15) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 14) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 16) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 14) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 15) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 16) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 14) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 16) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 14) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 16) (unit * (y0 + 4)) unit unit,
        -- S
        RF.RectF (unit * 19) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 20) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 18) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 19) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 19) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 20) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 19) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 18) (unit * (y0 + 4)) unit unit,
        -- U
        RF.RectF (unit * 22) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 24) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 22) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 24) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 22) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 24) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 22) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 24) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 23) (unit * (y0 + 4)) unit unit,
        -- N
        RF.RectF (unit * 26) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 26) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 26) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 26) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 26) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 27) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 28) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 29) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 29) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 29) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 29) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 29) (unit * (y0 + 4)) unit unit,
        -- O
        RF.RectF (unit * 32) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 31) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 33) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 31) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 33) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 31) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 33) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 32) (unit * (y0 + 4)) unit unit,
        -- R
        RF.RectF (unit * 35) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 35) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 35) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 35) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 35) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 36) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 36) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 37) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 37) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 37) (unit * (y0 + 4)) unit unit,
        -- I
        RF.RectF (unit * 39) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 41) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 0)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 1)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 2)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 3)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 39) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 40) (unit * (y0 + 4)) unit unit,
        RF.RectF (unit * 41) (unit * (y0 + 4)) unit unit
      ]

  let nG = GM.length rectsG
  speedsG <- UM.replicate nG $ VF.Vec2F 0 0
  aliveG <- UM.replicate nG True
  return Game {..}

input :: (MonadIO m, PrimMonad m) => Game (PrimState m) -> m ()
input Game {..} = do
  -- FIXME: enter is not working?
  isEnter <- liftIO $ (||) <$> R.isKeyDown R.KeyKpEnter <*> R.isKeyDown R.KeySpace
  lastIsStarted <- GM.read isStartedG 0
  GM.modify isStartedG (isEnter ||) 0
  isStarted <- GM.read isStartedG 0

  -- on start
  when (not lastIsStarted && isStarted) $ do
    GM.write speedsG ball $ VF.Vec2F initialBallSpeed (-initialBallSpeed)

  -- move yasunori
  when isStarted $ do
    l <- liftIO $ (bool 0 (-1) .) . (||) <$> R.isKeyDown R.KeyH <*> R.isKeyDown R.KeyLeft
    r <- liftIO $ (bool 0 1 .) . (||) <$> R.isKeyDown R.KeyL <*> R.isKeyDown R.KeyRight
    let dx = (l + r) * barSpeed
    GM.write speedsG yasunori $ VF.Vec2F dx 0

update :: (MonadIO m, PrimMonad m) => Game (PrimState m) -> m ()
update game@Game{..} = do
  whenM (GM.read isStartedG 0) $ do
    unlessM (handleDeath game) $ do
      doMove game
      handleCollision game

-- | \(O(1)\)
doMove :: (MonadIO m, PrimMonad m) => Game (PrimState m) -> m ()
doMove game@Game {..} = do
  FrozenGame {..} <- freeze game
  U.iforM_ (U.zip aliveFG speedsFG) $ \i (!b, !spd) -> do
    when b $ do
      rect <- GM.read rectsG i
      GM.write rectsG i $! RF.RectF (RF.x rect + VF.x spd) (RF.y rect + VF.y spd) (RF.w rect) (RF.h rect)

-- | \(O(1)\)
handleDeath :: (MonadIO m, PrimMonad m) => Game (PrimState m) -> m Bool
handleDeath game@Game {..} = do
  isDead <- GM.read isDeadG 0
  rect <- GM.read rectsG ball
  GM.write isDeadG 0 $! isDead || RF.d rect >= boundsH
  GM.read isDeadG 0

-- | \(O(n^2)\).
handleCollision :: (MonadIO m, PrimMonad m) => Game (PrimState m) -> m ()
handleCollision game@Game {..} = do
  FrozenGame {..} <- freeze game
  U.iforM_ (U.zip3 aliveFG rectsFG speedsFG) $ \i1 (!b1, !rect1, !spd1) -> do
    when b1 $ do
      U.iforM_ (U.zip aliveFG rectsFG) $ \i2 (!b2, !rect2) -> do
        -- Oh no, we're only interested in the ball after all. We should've name it yasunori instead.
        when (b2 && i1 /= i2 && (i1 == ball || i2 == ball)) $ do
          when (RF.intersects rect1 rect2) $ do
            if isErasableBlock i1
              then do
                -- HACK: Because we iterate the ball first, we can safely erase the block:
                GM.write aliveG i1 False
                liftIO $ putStrLn $ "erase " ++ show i1
              else do
                -- flip direction
                let eps = 2.0 :: Double
                let sgnX = bool 1 (-1) $ or [abs (a - b) <= eps | a <- [RF.l rect1, RF.r rect1], b <- [RF.l rect2, RF.r rect2]]
                let sgnY = bool 1 (-1) $ or [abs (a - b) <= eps | a <- [RF.u rect1, RF.d rect1], b <- [RF.u rect2, RF.d rect2]]
                GM.write speedsG i1 $ VF.Vec2F (sgnX * VF.x spd1) (sgnY * VF.y spd1)
                liftIO $ putStrLn $ "flip " ++ show (i1, i2, (sgnX, sgnY))

draw :: (MonadIO m, PrimMonad m) => Game (PrimState m) -> m ()
draw game@Game {..} = do
  FrozenGame {..} <- freeze game
  U.forM_ (U.zip3 aliveFG speedsFG rectsFG) $ \(!b, !spd, RF.RectF {..}) -> do
    when b $ do
      -- FIXME: pixel-perfect rendering
      liftIO $ R.drawRectangle (floor x) (floor y) (floor w) (floor h) R.white

  whenM (GM.read isDeadG 0) $ do
    liftIO $ R.drawText "You don't deserve yasunori." 30 30 64 R.lightGray
