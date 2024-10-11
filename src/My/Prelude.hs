-- | Bulk aggressive re-exports of pre-installed and selected packages.
module My.Prelude
  ( module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Primitive,
    module Control.Monad.Trans,
    module Data.Word,
    module Debug.Trace
  )
where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Trans
import Data.Word
import Debug.Trace
