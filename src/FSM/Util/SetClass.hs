{-# LANGUAGE QuantifiedConstraints #-}
module FSM.Util.SetClass where

import Prelude
import qualified Data.Set as S
import qualified FSM.Util.MultiSet as MS

class (forall a. Ord a => Monoid (s a)) => SetClass s where
    empty :: Ord a => s a
    empty = mempty
    singleton :: a -> s a
    union :: Ord a => s a -> s a -> s a
    union = (<>)
    difference :: Ord a => s a -> s a -> s a

instance SetClass [] where
    singleton a = [a]
    difference a b = filter (`notElem` b) a

instance SetClass S.Set where
    singleton = S.singleton
    difference = S.difference

instance SetClass MS.MultiSet where
    singleton = MS.singleton
    difference = MS.difference

