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

