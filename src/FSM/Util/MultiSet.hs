{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

A simple multiset data structure: an element can occur multiple times,
but the ordering is irrelevant.
|-}
module FSM.Util.MultiSet where

import Prelude
import qualified Data.Map.Strict as M

newtype MultiSet a = MultiSet (M.Map a Int)

empty :: MultiSet a
empty = MultiSet $ M.empty

singleton :: a -> MultiSet a
singleton a = MultiSet $ M.singleton a 1

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet m) (MultiSet m') = MultiSet $ M.unionWith (+) m m'

difference :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
difference (MultiSet m) (MultiSet m') = MultiSet $ M.difference m m'

lookup :: Ord a => a -> MultiSet a -> Int
lookup k (MultiSet m) = maybe 0 id $ M.lookup k m

instance Ord a => Semigroup (MultiSet a) where
    (<>) = union

instance Ord a => Monoid (MultiSet a) where
    mempty = empty

