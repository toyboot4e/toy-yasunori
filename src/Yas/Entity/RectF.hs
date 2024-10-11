-- | Unboxed rectangle.
module Yas.Entity.RectF (RectF (..), l, r, u, d, intersects, intersectsX, intersectsY) where

import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM

-- | Unboxed rectangle.
data RectF = RectF
  { x :: {-# UNPACK #-} !Double,
    y :: {-# UNPACK #-} !Double,
    w :: {-# UNPACK #-} !Double,
    h :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Show)

-- | Inclusive
l :: RectF -> Double
l RectF {x} = x

-- | TODO: Exclusive
r :: RectF -> Double
r RectF {x, w} = x + w

-- | Inclusive
u :: RectF -> Double
u RectF {y} = y

-- | TODO: Exclusive
d :: RectF -> Double
d RectF {y, h} = y + h

is :: Double -> Double -> Double -> Double -> Bool
is x1 x2 x1' x2' = not (x2 < x1' || x2' < x1)

intersects :: RectF -> RectF -> Bool
intersects r1 r2 = intersectsX r1 r2 && intersectsY r1 r2

intersectsX :: RectF -> RectF -> Bool
intersectsX r1 r2 = is (l r1) (r r1) (l r2) (r r2)

intersectsY :: RectF -> RectF -> Bool
intersectsY r1 r2 = is (u r1) (d r1) (u r2) (d r2)

type RectRepr = (Double, Double, Double, Double)

instance U.IsoUnbox RectF RectRepr where
  {-# INLINE toURepr #-}
  toURepr (RectF x y w h) = (x, y, w, h)
  {-# INLINE fromURepr #-}
  fromURepr (!x, !y, !w, !h) = RectF x y w h

newtype instance U.MVector s RectF = MV_RectF (UM.MVector s RectRepr)

newtype instance U.Vector RectF = V_RectF (U.Vector RectRepr)

deriving via (RectF `U.As` RectRepr) instance GM.MVector UM.MVector RectF

deriving via (RectF `U.As` RectRepr) instance G.Vector U.Vector RectF

instance U.Unbox RectF
