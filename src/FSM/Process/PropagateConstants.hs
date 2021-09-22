module FSM.Process.PropagateConstants(propagateConstants, propagateConstantsN) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

hasAssigns :: TH.Name -> Stmt -> Bool
hasAssigns n (SIf _ st sf) = hasAssigns n st || hasAssigns n sf
hasAssigns n (SCase _ cs) = any (hasAssigns n . snd) cs
hasAssigns _ (SRet _) = False
hasAssigns n (SBlock ss) = any (hasAssigns n) ss
hasAssigns _ (SYield _) = False
hasAssigns n (SAssign n' _) = n == n'
hasAssigns n (SFun fs s) = hasAssigns n s || any (\(p, s') -> not (n `S.member` patBound (freeVarsPat p)) && hasAssigns n s') fs
hasAssigns _ SNop = False
hasAssigns n (SLet _ n' _ s) = n /= n' && hasAssigns n s

setVars :: VarKind -> S.Set TH.Name -> M.Map TH.Name VarKind -> M.Map TH.Name VarKind
setVars t s m = M.fromList (map (, t) $ S.toList s) `M.union` m

propagateConstantsFunMap :: M.Map TH.Name VarKind -> FunMap -> FunMap
propagateConstantsFunMap m = M.map (propagateConstantsCase m)

propagateConstantsCase :: M.Map TH.Name VarKind -> (TH.Pat, Stmt) -> (TH.Pat, Stmt)
propagateConstantsCase m (p, s) = (p, propagateConstantsStmt (setVars VarLet (boundVars p) m) s)

propagateConstantsStmt :: M.Map TH.Name VarKind -> Stmt -> Stmt
propagateConstantsStmt m   (SIf e st sf) = SIf e (propagateConstantsStmt m st) (propagateConstantsStmt m sf)
propagateConstantsStmt m   (SCase e cs) = SCase e (map (propagateConstantsCase m) cs)
propagateConstantsStmt _ s@(SRet _) = s
propagateConstantsStmt m   (SBlock ss) = SBlock $ map (propagateConstantsStmt m) ss
propagateConstantsStmt _ s@(SYield _) = s
propagateConstantsStmt _ s@(SAssign _ _) = s
propagateConstantsStmt m   (SFun fs s) = SFun (propagateConstantsFunMap m fs) (propagateConstantsStmt m s)
propagateConstantsStmt _ s@SNop = s
propagateConstantsStmt m   (SLet t n vs s)
    | VExp e@(TH.VarE n') <- vs, Just VarLet <- M.lookup n' m, canSubst t = propagateConstantsStmt m $ substSingle n e s
    | VExp e <- vs, isConstantExpr e, canSubst t = propagateConstantsStmt m $ substSingle n e s
    | otherwise = SLet t n vs $ propagateConstantsStmt (M.insert n t m) s
    where
    canSubst VarLet = True
    canSubst VarMut = not $ hasAssigns n s

propagateConstants :: Prog -> Prog
propagateConstants prog = prog { progBody = propagateConstantsStmt m $ progBody prog }
    where
    m = setVars VarMut (boundVars $ progInputs prog) $ setVars VarLet (freeVars $ progBody prog) M.empty

propagateConstantsN :: NProg -> NProg
propagateConstantsN prog = prog { nProgFuns = propagateConstantsFunMap m $ nProgFuns prog }
    where
    m = setVars VarMut (boundVars $ nProgInputs prog) $ setVars VarLet (freeVarsFunMap $ nProgFuns prog) M.empty

