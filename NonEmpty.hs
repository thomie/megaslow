module NonEmpty where

import qualified Data.List           as List

infixr 5 :|

data NonEmpty a = a :| [a]
  deriving ( Eq, Ord, Show, Read )

fromList :: [a] -> NonEmpty a
fromList (a:as) = a :| as
-- fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"

toList :: NonEmpty a -> [a]
toList ~(a :| as) = a : as

init :: NonEmpty a -> [a]
init ~(a :| as) = List.init (a : as)

last :: NonEmpty a -> a
last ~(a :| as) = List.last (a : as)

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (a:as) = Just (a :| as)

class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

instance Semigroup [a] where
  (<>) = (++)
