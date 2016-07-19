-- |
-- Module      :  Text.Megaparsec.Char
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Commonly used character parsers.

{-# LANGUAGE TypeFamilies     #-}

module Char where

import Control.Applicative
import qualified Data.Set as E

import Combinator
import Error
import Prim

import NonEmpty

anyChar :: (MonadParsec e s m, Token s ~ Char) => m Char
anyChar = satisfy (const True) -- <?> "character"
{-# INLINE anyChar #-}

satisfy :: (MonadParsec e s m, Token s ~ Char) => (Char -> Bool) -> m Char
satisfy f = token testChar Nothing
  where
    testChar x =
      if f x
        then Right x
        else Left (E.singleton (Tokens (x:|[])), E.empty, E.empty)
{-# INLINE satisfy #-}
