module Mtl (
    module Control.Monad.Trans.State.Lazy,
    Identity,
    lift
) where

import Control.Monad.Trans.State.Lazy (StateT(..))
import Data.Functor.Identity

import Prelude hiding (undefined, error)
import NoCallStack

lift :: Monad m => m a -> t m a 
lift = undefined
