{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>
|-}
{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.DesugarOutputs(desugarOutputs) where

import Prelude
import FSM.Lang
import Control.Monad.Reader
import Control.Lens
import Control.Arrow
import Data.Key(mapWithKeyM)
import Data.Maybe(fromJust, isJust)
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh
import FSM.Util.Qlift

data RData = RData {
    _rDataOutputs :: [(TH.Name, Output)],
    _rDataNames :: M.Map TH.Name TH.Name
}

$(makeLenses ''RData)

makeOutputs :: MonadReader RData m => m [TH.Exp]
makeOutputs = map (TH.VarE . fst) <$> view rDataOutputs

restoreDefaults :: MonadReader RData m => m [Stmt LvlLoops]
restoreDefaults = map f <$> view rDataOutputs
    where
    f (n, o) = SAssign n $ outputDefault o

mkAssigns :: [TH.Name] -> TH.Exp -> [Stmt LvlLoops]
mkAssigns []  _ = []
mkAssigns [n] e = [SAssign n e]
mkAssigns ns  (TH.TupE es)
    | length es == length ns && all isJust es = zipWith SAssign ns (map fromJust es)
    | otherwise = error "Invalid number of outputs in yield"
mkAssigns _ _ = error "Invalid yield call"

varNames :: MonadReader RData m => [TH.Name] -> m [TH.Name]
varNames ns = (\nm -> map (fromJust . flip M.lookup nm) ns) <$> view rDataNames

desugarOutputsStmt :: (Qlift m, MonadRefresh m, MonadReader RData m) => Stmt LvlSugared -> m (Stmt LvlLoops)
desugarOutputsStmt (SLoop lt s) = SLoop lt <$> desugarOutputsStmt s
desugarOutputsStmt (SBreak bt) = return $ SBreak bt
desugarOutputsStmt (SFun fs s) = SFun <$> mapM (\(p, s') -> (p,) <$> desugarOutputsStmt s') fs <*> desugarOutputsStmt s
desugarOutputsStmt (SLet t n vs s) = SLet t n vs <$> desugarOutputsStmt s
desugarOutputsStmt (SAssign n e) = return $ SAssign n e
desugarOutputsStmt (SOutput ns e) = do
    ns' <- varNames ns
    return $ sBlockS $ mkAssigns ns' e
desugarOutputsStmt (SYieldO ns e) = do
    ns' <- varNames ns
    rds <- restoreDefaults
    os <- makeOutputs
    return $ sBlockS $ mkAssigns ns' e ++ SYield (tupE os) : rds
desugarOutputsStmt (SRet vs) = return $ SRet vs
desugarOutputsStmt (SBlock ss) = SBlock <$> mapM desugarOutputsStmt ss
desugarOutputsStmt (SIf e st sf) = SIf e <$> desugarOutputsStmt st <*> desugarOutputsStmt sf
desugarOutputsStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> desugarOutputsStmt s) cs
desugarOutputsStmt (SNop) = return SNop

outputVar :: (TH.Name, Output) -> Stmt LvlLoops -> Stmt LvlLoops
outputVar (n, o) s = SLet VarMut n (VExp $ outputDefault o) s

desugarOutputs :: (Qlift m, MonadRefresh m) => Prog LvlSugared -> m (Prog LvlLoops)
desugarOutputs prog = do
    ns <- mapWithKeyM (\k _ -> refreshName k) $ M.fromList $ progOutputs prog
    let os = map (fromJust . flip M.lookup ns *** id) $ progOutputs prog
    b <- flip runReaderT (RData os ns) $ desugarOutputsStmt $ progBody prog
    return $ prog { progBody = foldr outputVar b os, progOutputs = [] }


