{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.DeTailCall(deTailCall) where

import Prelude
import FSM.Lang
import Control.Lens
import Control.Monad.Reader
import Data.Key(mapWithKeyM)
import qualified Data.Set as S
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh
import FSM.Process.ReturningFuns
import FSM.Process.TailCallSCC

data DTData = DTData {
    _dtDataFunction :: TH.Name,
    _dtDataReturning :: S.Set TH.Name,
    _dtDataSCC :: Partition TH.Name
}

$(makeLenses ''DTData)

deTailCall :: (IsDesugared l, WithNop l, MonadRefresh m) => Prog l -> m (Prog l)
deTailCall prog = do
    s' <- flip runReaderT (DTData (TH.mkName "") (returningFuns $ progBody prog) (tailCallSCC prog)) $ deTailCallStmt $ progBody prog
    return $ prog { progBody = s' }

deTailCallStmt :: (IsDesugared l, WithNop l, MonadReader DTData m, MonadRefresh m) => Stmt l -> m (Stmt l)
deTailCallStmt s@(SNop) = return s
deTailCallStmt s@(SYield _) = return s
deTailCallStmt   (SLet t n e s) = SLet t n e <$> deTailCallStmt s
deTailCallStmt s@(SAssign _ _) = return s
deTailCallStmt   (SBlock ss) = SBlock <$> mapM deTailCallStmt ss
deTailCallStmt   (SIf e st sf) = SIf e <$> deTailCallStmt st <*> deTailCallStmt sf
deTailCallStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> deTailCallStmt s) cs
deTailCallStmt s@(SRet (VExp _)) = return s
deTailCallStmt s@(SRet (VCall f e)) = do
    rf <- view dtDataReturning
    f' <- view dtDataFunction
    part <- view dtDataSCC
    if f `S.member` rf && partitionLookup f part /= partitionLookup f' part
    then do
        n <- refreshName f
        return $ SLet VarLet  n (VCall f e) (SRet (VExp $ TH.VarE n))
    else return s
deTailCallStmt   (SFun fs s) =
    SFun <$> mapWithKeyM (\f (p, s') -> locally dtDataFunction (const f) $ (p,) <$> deTailCallStmt s') fs <*> deTailCallStmt s


