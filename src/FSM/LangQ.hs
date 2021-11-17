module FSM.LangQ where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import FSM.Lang
import Prelude

type VStmtQ = TH.Q VStmt

type StmtQ = TH.Q Stmt

type FunMapQ = M.Map TH.Name (TH.PatQ, StmtQ)

psequence :: Monad m => (m a, m b) -> m (a, b)
psequence (a, b) = (,) <$> a <*> b

vExp :: TH.ExpQ -> VStmtQ
vExp e = VExp <$> e

vCall :: TH.Name -> TH.ExpQ -> VStmtQ
vCall n e = VCall n <$> e

sLet :: VarKind -> TH.Name -> VStmtQ -> StmtQ -> StmtQ
sLet k n vs s = SLet k n <$> vs <*> s

sAssign :: TH.Name -> TH.ExpQ -> StmtQ
sAssign n e = SAssign n <$> e

sYield :: TH.ExpQ -> StmtQ
sYield e = SYield <$> e

sRet :: VStmtQ -> StmtQ
sRet vs = SRet <$> vs

sFun :: FunMapQ -> StmtQ -> StmtQ
sFun fs s = SFun <$> sequence (M.map psequence fs) <*> s

sBlock :: [StmtQ] -> StmtQ
sBlock ss = SBlock <$> sequence ss

sIf :: TH.ExpQ -> StmtQ -> StmtQ -> StmtQ
sIf e st sf = SIf <$> e <*> st <*> sf

sCase :: TH.ExpQ -> [(TH.PatQ, StmtQ)] -> StmtQ
sCase e cs = SCase <$> e <*> sequence (map psequence cs)

sNop :: StmtQ
sNop = return SNop


