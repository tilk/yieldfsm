{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

YieldFSM ADT constructors in the Template Haskell Q monad.
-}
module FSM.LangQ where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import FSM.Lang
import Prelude

-- | Evaluable in Q monad.
type VStmtQ = TH.Q VStmt

-- | Statement in Q monad.
type StmtQ l = TH.Q (Stmt l)

-- | Set of functions in Q monad.
type FunMapQ l = M.Map TH.Name (TH.PatQ, StmtQ l)

-- | Pair constructor in Q monad.
psequence :: Monad m => (m a, m b) -> m (a, b)
psequence (a, b) = (,) <$> a <*> b

-- | Expression evaluable in Q monad.
vExp :: TH.ExpQ -> VStmtQ
vExp e = VExp <$> e

-- | Function call evaluable in Q monad.
vCall :: TH.Name -> TH.ExpQ -> VStmtQ
vCall n e = VCall n <$> e

-- | Let statement in Q monad.
sLet :: VarKind -> TH.Name -> VStmtQ -> StmtQ l -> StmtQ l
sLet k n vs s = SLet k n <$> vs <*> s

-- | Mutable variable assignment statement in Q monad.
sAssign :: WithAssign l => TH.Name -> TH.ExpQ -> StmtQ l
sAssign n e = SAssign n <$> e

-- | Yield statement in Q monad.
sYield :: (WithBlock l, NoOutputs l) => TH.ExpQ -> StmtQ l
sYield e = SYield <$> e

-- | Return statement in Q monad.
sRet :: VStmtQ -> StmtQ l
sRet vs = SRet <$> vs

-- | Function definition statement in Q monad.
sFun :: WithFun l => FunMapQ l -> StmtQ l -> StmtQ l
sFun fs s = SFun <$> sequence (M.map psequence fs) <*> s

-- | Statement composition in Q monad.
sBlock :: WithBlock l => [StmtQ l] -> StmtQ l
sBlock ss = SBlock <$> sequence ss

-- | If statement in Q monad.
sIf :: TH.ExpQ -> StmtQ l -> StmtQ l -> StmtQ l
sIf e st sf = SIf <$> e <*> st <*> sf

-- | Case statement in Q monad.
sCase :: TH.ExpQ -> [(TH.PatQ, StmtQ l)] -> StmtQ l
sCase e cs = SCase <$> e <*> sequence (map psequence cs)

-- | Skip statement in Q monad.
sNop :: WithBlock l => StmtQ l
sNop = return SNop


