-- | Unboxed 2D vector.
module Yas.Entity.Vec2F (Vec2F(..), zero, negate) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Prelude hiding (negate)

-- | Unboxed 2D vector.
data Vec2F = Vec2F
  { x :: {-# UNPACK #-} !Double,
    y :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Show)

zero :: Vec2F
zero = Vec2F 0 0

negate :: Vec2F -> Vec2F
negate Vec2F {..} = Vec2F (-x) (-y)

type Vec2FRepr = (Double, Double)

instance U.IsoUnbox Vec2F Vec2FRepr where
  {-# INLINE toURepr #-}
  toURepr (Vec2F x y) = (x, y)
  {-# INLINE fromURepr #-}
  fromURepr (!x, !y) = Vec2F x y

newtype instance U.MVector s Vec2F = MV_Vec2F (UM.MVector s Vec2FRepr)
newtype instance U.Vector Vec2F = V_Vec2F (U.Vector Vec2FRepr)

deriving via (Vec2F `U.As` Vec2FRepr) instance GM.MVector UM.MVector Vec2F

deriving via (Vec2F `U.As` Vec2FRepr) instance G.Vector U.Vector Vec2F

instance U.Unbox Vec2F
