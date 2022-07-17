{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Monads which contain `TH.Q`.

This module is only required because the Quote class is not available in GHC 8.10.
|-}
module FSM.Util.Qlift where

import qualified Language.Haskell.TH as TH
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Prelude

class Monad m => Qlift m where
    qlift :: TH.Q a -> m a

instance Qlift TH.Q where
    qlift = id

instance Qlift IO where
    qlift = TH.runQ

instance Qlift m => Qlift (ReaderT a m) where
    qlift = lift . qlift

instance (Monoid a, Qlift m) => Qlift (WriterT a m) where
    qlift = lift . qlift

instance Qlift m => Qlift (StateT a m) where
    qlift = lift . qlift

