module FSM.FreeVars where

import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Prelude
import Data.Maybe(maybe)
import FSM.Lang

data PatFV = PatFV (S.Set TH.Name) (S.Set TH.Name)

patEmpty = PatFV S.empty S.empty
patSingleton n = PatFV (S.singleton n) S.empty

patUnion (PatFV s1 s2) (PatFV t1 t2) = PatFV (s1 `S.union` t1) (s2 `S.union` t2)

patUnions = foldr patUnion patEmpty

underPat s (PatFV s1 s2) = s2 `S.union` (s `S.difference` s1)

freeVarsExpMaybe :: Maybe TH.Exp -> S.Set TH.Name
freeVarsExpMaybe = maybe S.empty id . fmap freeVarsExp

freeVarsExp :: TH.Exp -> S.Set TH.Name
freeVarsExp (TH.VarE v) = S.singleton v
freeVarsExp (TH.ConE _) = S.empty
freeVarsExp (TH.LitE _) = S.empty
freeVarsExp (TH.AppE e1 e2) = freeVarsExp e1 `S.union` freeVarsExp e2
freeVarsExp (TH.AppTypeE e _ ) = freeVarsExp e
freeVarsExp (TH.InfixE me1 e me2) = freeVarsExp e `S.union` freeVarsExpMaybe me1 `S.union` freeVarsExpMaybe me2
freeVarsExp (TH.UInfixE e1 e e2) = freeVarsExp e `S.union` freeVarsExp e1 `S.union` freeVarsExp e2
freeVarsExp (TH.ParensE e) = freeVarsExp e
freeVarsExp (TH.LamE ps e) = freeVarsExp e `underPat` patUnions (map freeVarsPat ps)
freeVarsExp (TH.LamCaseE ms) = S.unions $ map freeVarsMatch ms
freeVarsExp (TH.TupE es) = S.unions $ map freeVarsExpMaybe es
freeVarsExp (TH.UnboxedTupE es) = S.unions $ map freeVarsExpMaybe es
freeVarsExp (TH.UnboxedSumE e _ _) = freeVarsExp e
freeVarsExp (TH.CondE e e1 e2) = freeVarsExp e `S.union` freeVarsExp e1 `S.union` freeVarsExp e2
-- MultiIfE
-- LetE
-- CaseE
-- DoE
-- MDoE
-- CompE
-- ArithE
freeVarsExp (TH.ListE es) = S.unions $ map freeVarsExp es
freeVarsExp (TH.SigE e _) = freeVarsExp e
-- RecConE
-- RecUpdE
-- StaticE
-- UnboundVarE
freeVarsExp (TH.LabelE _) = S.empty

freeVarsFieldPat :: TH.FieldPat -> PatFV
freeVarsFieldPat (n, p) = freeVarsPat p

freeVarsPat :: TH.Pat -> PatFV
freeVarsPat (TH.LitP _) = patEmpty
freeVarsPat (TH.VarP n) = patSingleton n
freeVarsPat (TH.TupP ps) = patUnions $ map freeVarsPat ps
freeVarsPat (TH.UnboxedTupP ps) = patUnions $ map freeVarsPat ps
freeVarsPat (TH.UnboxedSumP p _ _) = freeVarsPat p
freeVarsPat (TH.ConP _ ps) = patUnions $ map freeVarsPat ps
freeVarsPat (TH.InfixP p1 _ p2) = freeVarsPat p1 `patUnion` freeVarsPat p2
freeVarsPat (TH.UInfixP p1 _ p2) = freeVarsPat p1 `patUnion` freeVarsPat p2
freeVarsPat (TH.ParensP p) = freeVarsPat p
freeVarsPat (TH.TildeP p) = freeVarsPat p
freeVarsPat (TH.BangP p) = freeVarsPat p
freeVarsPat (TH.AsP n p) = freeVarsPat p `patUnion` patSingleton n
freeVarsPat (TH.WildP) = patEmpty
freeVarsPat (TH.RecP _ fps) = patUnions $ map freeVarsFieldPat fps
freeVarsPat (TH.ListP ps) = patUnions $ map freeVarsPat ps
freeVarsPat (TH.SigP p _) = freeVarsPat p
freeVarsPat (TH.ViewP e p) = freeVarsPat p `patUnion` PatFV S.empty (freeVarsExp e)

freeVarsDec :: TH.Dec -> PatFV
freeVarsDec _ = undefined

freeVarsBody :: TH.Body -> PatFV
freeVarsBody (TH.NormalB e) = PatFV S.empty (freeVarsExp e)
freeVarsBody (TH.GuardedB ges) = undefined

freeVarsMatch :: TH.Match -> S.Set TH.Name
freeVarsMatch (TH.Match p b ds) = undefined

freeVarsVStmt :: VStmt -> S.Set TH.Name
freeVarsVStmt (VExp e) = freeVarsExp e
freeVarsVStmt (VCall n e) = freeVarsExp e

freeVarsStmt :: Stmt -> S.Set TH.Name
freeVarsStmt (SLet _ v vs s) = freeVarsVStmt vs `S.union` (freeVarsStmt s `underPat` freeVarsPat (TH.VarP v))
freeVarsStmt (SAssign v e) = freeVarsExp e
freeVarsStmt (SEmit e) = freeVarsExp e
freeVarsStmt (SRet vs) = freeVarsVStmt vs
freeVarsStmt (SFun fs s) = freeVarsStmt s `S.union` S.unions (flip map (M.toList fs) $ \(_, (p, s)) -> freeVarsStmt s `underPat` freeVarsPat p)
freeVarsStmt (SBlock ss) = S.unions $ map freeVarsStmt ss
freeVarsStmt (SIf e s1 s2) = freeVarsExp e `S.union` freeVarsStmt s1 `S.union` freeVarsStmt s2
freeVarsStmt (SCase e cs) = freeVarsExp e `S.union` S.unions (flip map cs $ \(p, s) -> freeVarsStmt s `underPat` freeVarsPat p)
freeVarsStmt (SNop) = S.empty

freeVarsStmts :: [Stmt] -> S.Set TH.Name
freeVarsStmts ss = S.unions $ map freeVarsStmt ss

renameName :: M.Map TH.Name TH.Name -> TH.Name -> TH.Name
renameName s n | Just n' <- M.lookup n s = n'
               | otherwise = n

renameExp :: M.Map TH.Name TH.Name -> TH.Exp -> TH.Exp
renameExp s e@(TH.VarE v) = TH.VarE (renameName s v)
renameExp s e@(TH.ConE _) = e
renameExp s e@(TH.LitE _) = e
renameExp s   (TH.AppE e1 e2) = TH.AppE (renameExp s e1) (renameExp s e2)
renameExp s   (TH.AppTypeE e t) = TH.AppTypeE (renameExp s e) t
renameExp s   (TH.InfixE me1 e me2) = TH.InfixE (renameExp s <$> me1) (renameExp s e) (renameExp s <$> me2)
renameExp s   (TH.UInfixE e1 e e2) = TH.UInfixE (renameExp s e1) (renameExp s e) (renameExp s e2)
renameExp s   (TH.ParensE e) = TH.ParensE (renameExp s e)
renameExp s   (TH.LamE ps e) = TH.LamE (renamePat s <$> ps) (renameExp s' e)
    where s' = cutSubst (patUnions $ map freeVarsPat ps) s
renameExp s   (TH.TupE es) = TH.TupE (fmap (renameExp s) <$> es)

renameFieldPat :: M.Map TH.Name TH.Name -> TH.FieldPat -> TH.FieldPat
renameFieldPat s (n, p) = (n, renamePat s p)

renamePat :: M.Map TH.Name TH.Name -> TH.Pat -> TH.Pat
renamePat s p@(TH.LitP _) = p
renamePat s p@(TH.VarP n) = p
renamePat s   (TH.TupP ps) = TH.TupP (renamePat s <$> ps)
renamePat s   (TH.UnboxedTupP ps) = TH.UnboxedTupP (renamePat s <$> ps)
renamePat s   (TH.ConP n ps) = TH.ConP n (renamePat s <$> ps)
renamePat s   (TH.InfixP p1 n p2) = TH.InfixP (renamePat s p1) n (renamePat s p2)
renamePat s   (TH.UInfixP p1 n p2) = TH.UInfixP (renamePat s p1) n (renamePat s p2)
renamePat s   (TH.ParensP p) = TH.ParensP (renamePat s p)
renamePat s   (TH.TildeP p) = TH.TildeP (renamePat s p)
renamePat s   (TH.BangP p) = TH.BangP (renamePat s p)
renamePat s   (TH.AsP n p) = TH.AsP n (renamePat s p)
renamePat s p@(TH.WildP) = p
renamePat s   (TH.RecP n fps) = TH.RecP n (renameFieldPat s <$> fps)
renamePat s   (TH.ListP ps) = TH.ListP (renamePat s <$> ps)
renamePat s   (TH.SigP p t) = TH.SigP (renamePat s p) t
renamePat s   (TH.ViewP e p) = TH.ViewP (renameExp s e) (renamePat s p)

cutSubst :: PatFV -> M.Map TH.Name a -> M.Map TH.Name a
cutSubst (PatFV vs _) s = M.withoutKeys s vs

renameStmt :: M.Map TH.Name TH.Name -> Stmt -> Stmt
renameStmt su   (SLet t v vs s) = SLet t v (renameVStmt su' vs) (renameStmt su' s)
    where su' = cutSubst (patSingleton v) su
renameStmt su   (SAssign v e) = SAssign (renameName su v) (renameExp su e)
renameStmt su   (SEmit e) = SEmit (renameExp su e)
renameStmt su   (SRet vs) = SRet (renameVStmt su vs)
renameStmt su   (SFun fs s) = SFun (flip M.map fs $ \(p, s) -> (renamePat su p, renameStmt (cutSubst (freeVarsPat p) su) s)) (renameStmt su s)
renameStmt su   (SBlock ss) = SBlock (renameStmt su <$> ss)
renameStmt su   (SIf e st sf) = SIf (renameExp su e) (renameStmt su st) (renameStmt su sf)
renameStmt su   (SCase e cs) = SCase (renameExp su e) (flip map cs $ \(p, s) -> (renamePat su p, renameStmt (cutSubst (freeVarsPat p) su) s))
renameStmt su s@(SNop) = s

renameVStmt :: M.Map TH.Name TH.Name -> VStmt -> VStmt
renameVStmt su (VExp e) = VExp (renameExp su e)
renameVStmt su (VCall n e) = VCall n (renameExp su e)

