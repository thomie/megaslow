{-# LANGUAGE MagicHash #-}

-- Use this module instead of Prelude.undefined/error, to workaround #12150
-- ("Compile time performance degradation on code that uses undefined/error
--   with CallStacks")

module NoCallStack where

import GHC.Exception (errorCallException)
import GHC.Prim (raise#)
import Prelude (Char)

error :: [Char] -> a
error s = raise# (errorCallException s)

undefined :: a
undefined = error "undefined without callstack"
