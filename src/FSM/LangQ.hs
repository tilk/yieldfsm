{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

YieldFSM ADT constructors in the Template Haskell Q monad.
|-}
module FSM.LangQ where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import FSM.Lang
import Prelude

type VStmtQ = TH.Q VStmt

type StmtQ l = TH.Q (Stmt l)

type FunMapQ l = M.Map TH.Name (TH.PatQ, StmtQ l)

psequence :: Monad m => (m a, m b) -> m (a, b)
psequence (a, b) = (,) <$> a <*> b

vExp :: TH.ExpQ -> VStmtQ
vExp e = VExp <$> e

vCall :: TH.Name -> TH.ExpQ -> VStmtQ
vCall n e = VCall n <$> e

sLet :: VarKind -> TH.Name -> VStmtQ -> StmtQ l -> StmtQ l
sLet k n vs s = SLet k n <$> vs <*> s

sAssign :: WithAssign l => TH.Name -> TH.ExpQ -> StmtQ l
sAssign n e = SAssign n <$> e

sYield :: (WithBlock l, NoOutputs l) => TH.ExpQ -> StmtQ l
sYield e = SYield <$> e

sRet :: VStmtQ -> StmtQ l
sRet vs = SRet <$> vs

sFun :: WithFun l => FunMapQ l -> StmtQ l -> StmtQ l
sFun fs s = SFun <$> sequence (M.map psequence fs) <*> s

sBlock :: WithBlock l => [StmtQ l] -> StmtQ l
sBlock ss = SBlock <$> sequence ss

sIf :: TH.ExpQ -> StmtQ l -> StmtQ l -> StmtQ l
sIf e st sf = SIf <$> e <*> st <*> sf

sCase :: TH.ExpQ -> [(TH.PatQ, StmtQ l)] -> StmtQ l
sCase e cs = SCase <$> e <*> sequence (map psequence cs)

sNop :: WithBlock l => StmtQ l
sNop = return SNop


