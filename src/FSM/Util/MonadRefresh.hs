{-# LANGUAGE FlexibleContexts #-}
module FSM.Util.MonadRefresh(MonadRefresh(..), makeSeqName, refreshName, refreshNameWithPrefix, refreshSeqNameWithPrefix, refreshPat) where

import Prelude
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadUnique

class Monad m => MonadRefresh m where
    makeName :: String -> m TH.Name

instance MonadRefresh TH.Q where
    makeName = TH.newName

instance (Monoid s, MonadRefresh m) => MonadRefresh (WriterT s m) where
    makeName = lift . makeName

instance MonadRefresh m => MonadRefresh (StateT s m) where
    makeName = lift . makeName

instance MonadRefresh m => MonadRefresh (ReaderT s m) where
    makeName = lift . makeName

instance MonadRefresh m => MonadRefresh (UniqueT m) where
    makeName = lift . makeName

makeSeqName :: (MonadUnique m, MonadRefresh m) => String -> m TH.Name
makeSeqName n = do
    a <- fresh
    makeName $ n ++ show a

refreshName :: MonadRefresh m => TH.Name -> m TH.Name
refreshName n = makeName $ TH.nameBase n

refreshNameWithPrefix :: MonadRefresh m => String -> TH.Name -> m TH.Name
refreshNameWithPrefix p n = makeName $ p ++ TH.nameBase n

refreshSeqNameWithPrefix :: (MonadUnique m, MonadRefresh m) => String -> TH.Name -> m TH.Name
refreshSeqNameWithPrefix p n = makeSeqName $ p ++ TH.nameBase n

refreshPat :: MonadRefresh m => TH.Pat -> m (TH.Pat, M.Map TH.Name TH.Name)
refreshPat = runWriterT . f where
    f p@(TH.LitP _) = return p
    f   (TH.VarP v) = do
        v' <- refreshName v
        writer (TH.VarP v', M.singleton v v')
    f   (TH.TupP ps) = TH.TupP <$> mapM f ps
    f   (TH.UnboxedTupP ps) = TH.UnboxedTupP <$> mapM f ps
    f   (TH.UnboxedSumP p x y) = TH.UnboxedSumP <$> f p <*> pure x <*> pure y
    f   (TH.ConP n ps) = TH.ConP n <$> mapM f ps
    f   (TH.InfixP p1 n p2) = TH.InfixP <$> f p1 <*> pure n <*> f p2
    f   (TH.UInfixP p1 n p2) = TH.UInfixP <$> f p1 <*> pure n <*> f p2
    f   (TH.ParensP p) = TH.ParensP <$> f p
    f   (TH.TildeP p) = TH.TildeP <$> f p
    f   (TH.BangP p) = TH.BangP <$> f p
    f   (TH.AsP v p) = do
        v' <- refreshName v
        p' <- f p
        writer (TH.AsP v' p', M.singleton v v')
    f p@(TH.WildP) = return p
    f   (TH.RecP n fps) = TH.RecP n <$> forM fps (\(n', p) -> (n',) <$> f p)
    f   (TH.ListP ps) = TH.ListP <$> mapM f ps
    f   (TH.SigP p t) = TH.SigP <$> f p <*> pure t
    f   (TH.ViewP e p) = TH.ViewP e <$> f p

