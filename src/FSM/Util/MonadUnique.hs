{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

A monad for generating unique identifiers.
|-}
module FSM.Util.MonadUnique(MonadUnique, UniqueT, evalUniqueT, fresh) where

import Prelude
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance Monad m => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

instance (Monoid s, MonadUnique m) => MonadUnique (WriterT s m) where
    fresh = lift fresh

instance MonadUnique m => MonadUnique (StateT s m) where
    fresh = lift fresh

instance MonadUnique m => MonadUnique (ReaderT s m) where
    fresh = lift fresh

evalUniqueT :: Monad m => UniqueT m a -> m a
evalUniqueT (UniqueT s) = evalStateT s 0

