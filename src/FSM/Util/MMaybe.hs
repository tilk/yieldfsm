{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

A `Maybe` type with two `Nothing`s, with different behavior with respect
to `<|>`.
|-}
module FSM.Util.MMaybe where

import Prelude
import Control.Applicative
import Control.Monad

data MMaybe a = MJust a | MNo | MNoNo

instance Functor MMaybe where
    fmap f (MJust a) = MJust (f a)
    fmap _ MNo = MNo
    fmap _ MNoNo = MNoNo

instance Applicative MMaybe where
    pure = MJust
    MJust f <*> MJust a = MJust (f a)
    MNoNo <*> _ = MNoNo
    _ <*> MNoNo = MNoNo
    MNo <*> _ = MNo
    _ <*> MNo = MNo

instance Monad MMaybe where
    MJust a >>= f = f a
    MNo >>= _ = MNo
    MNoNo >>= _ = MNoNo

instance Alternative MMaybe where
    empty = MNo
    MNoNo <|> _ = MNoNo
    _ <|> MNoNo = MNoNo
    (MJust a) <|> _ = MJust a
    MNo <|> m = m

instance MonadPlus MMaybe

mmaybe :: b -> (a -> b) -> MMaybe a -> b
mmaybe _ f (MJust a) = f a
mmaybe a _ MNo = a
mmaybe a _ MNoNo = a

