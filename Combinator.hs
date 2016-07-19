-- |
-- Module      :  Text.Megaparsec.Combinator
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly used generic combinators. Note that all combinators works with
-- any 'Alternative' instances.

module Combinator where

import Control.Applicative

manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = go where go = ([] <$ end) <|> ((:) <$> p <*> go)
{-# INLINE manyTill #-}

-- | @someTill p end@ works similarly to @manyTill p end@, but @p@ should
-- succeed at least once.

someTill :: Alternative m => m a -> m end -> m [a]
someTill p end = (:) <$> p <*> manyTill p end
{-# INLINE someTill #-}
