module FSM.FreeVars where

import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Prelude
import Data.Maybe(maybe)
import FSM.Lang

class FreeVars a where
    freeVars :: a -> S.Set TH.Name

class FreeVarsPat a where
    freeVarsPat :: a -> PatFV

data PatFV = PatFV { patBound :: S.Set TH.Name, patFree :: S.Set TH.Name }

patEmpty = PatFV S.empty S.empty
patSingleton n = PatFV (S.singleton n) S.empty

patUnion (PatFV s1 s2) (PatFV t1 t2) = PatFV (s1 `S.union` t1) (s2 `S.union` t2)

patUnions = foldr patUnion patEmpty

underPat s (PatFV s1 s2) = s2 `S.union` (s `S.difference` s1)

boundVars :: FreeVarsPat a => a -> S.Set TH.Name
boundVars = patBound . freeVarsPat

instance FreeVars a => FreeVars (Maybe a) where
    freeVars = maybe S.empty id . fmap freeVars

freeVarsFunMap :: FunMap -> S.Set TH.Name
freeVarsFunMap = S.unions . map (\(_, (p, s)) -> freeVars s `underPat` freeVarsPat p) . M.toList

instance FreeVars TH.Type where
    freeVars = const S.empty

instance FreeVars TH.Exp where
    freeVars (TH.VarE v) = S.singleton v
    freeVars (TH.ConE _) = S.empty
    freeVars (TH.LitE _) = S.empty
    freeVars (TH.AppE e1 e2) = freeVars e1 `S.union` freeVars e2
    freeVars (TH.AppTypeE e _ ) = freeVars e
    freeVars (TH.InfixE me1 e me2) = freeVars e `S.union` freeVars me1 `S.union` freeVars me2
    freeVars (TH.UInfixE e1 e e2) = freeVars e `S.union` freeVars e1 `S.union` freeVars e2
    freeVars (TH.ParensE e) = freeVars e
    freeVars (TH.LamE ps e) = freeVars e `underPat` patUnions (map freeVarsPat ps)
    freeVars (TH.LamCaseE ms) = freeVars ms
    freeVars (TH.TupE es) = freeVars es
    freeVars (TH.UnboxedTupE es) = freeVars es
    freeVars (TH.UnboxedSumE e _ _) = freeVars e
    freeVars (TH.CondE e e1 e2) = freeVars e `S.union` freeVars e1 `S.union` freeVars e2
    -- MultiIfE
    -- LetE
    -- CaseE
    -- DoE
    -- MDoE
    -- CompE
    -- ArithE
    freeVars (TH.ListE es) = freeVars es
    freeVars (TH.SigE e _) = freeVars e
    -- RecConE
    -- RecUpdE
    -- StaticE
    -- UnboundVarE
    freeVars (TH.LabelE _) = S.empty

freeVarsFieldPat (n, p) = freeVarsPat p

instance FreeVarsPat TH.Pat where
    freeVarsPat (TH.LitP _) = patEmpty
    freeVarsPat (TH.VarP n) = patSingleton n
    freeVarsPat (TH.TupP ps) = freeVarsPat ps
    freeVarsPat (TH.UnboxedTupP ps) = freeVarsPat ps
    freeVarsPat (TH.UnboxedSumP p _ _) = freeVarsPat p
    freeVarsPat (TH.ConP _ ps) = freeVarsPat ps
    freeVarsPat (TH.InfixP p1 _ p2) = freeVarsPat p1 `patUnion` freeVarsPat p2
    freeVarsPat (TH.UInfixP p1 _ p2) = freeVarsPat p1 `patUnion` freeVarsPat p2
    freeVarsPat (TH.ParensP p) = freeVarsPat p
    freeVarsPat (TH.TildeP p) = freeVarsPat p
    freeVarsPat (TH.BangP p) = freeVarsPat p
    freeVarsPat (TH.AsP n p) = freeVarsPat p `patUnion` patSingleton n
    freeVarsPat (TH.WildP) = patEmpty
    freeVarsPat (TH.RecP _ fps) = patUnions $ map freeVarsFieldPat fps
    freeVarsPat (TH.ListP ps) = freeVarsPat ps
    freeVarsPat (TH.SigP p _) = freeVarsPat p
    freeVarsPat (TH.ViewP e p) = freeVarsPat p `patUnion` PatFV S.empty (freeVars e)

instance FreeVars TH.Pat where
    freeVars = patFree . freeVarsPat

freeVarsDec :: TH.Dec -> PatFV
freeVarsDec _ = undefined

instance FreeVarsPat TH.Body where
    freeVarsPat (TH.NormalB e) = PatFV S.empty (freeVars e)
    freeVarsPat (TH.GuardedB ges) = undefined

instance FreeVars TH.Match where
    freeVars (TH.Match p b ds) = undefined

instance FreeVars TH.Stmt where
    freeVars (TH.BindS p e) = freeVars e `underPat` freeVarsPat p
    --freeVars (TH.LetS ds) = 
    freeVars (TH.NoBindS e) = freeVars e
    freeVars (TH.ParS sss) = freeVars sss
    freeVars (TH.RecS ss) = freeVars ss

instance FreeVars VStmt where
    freeVars (VExp e) = freeVars e
    freeVars (VCall n e) = freeVars e

instance FreeVars Stmt where
    freeVars (SLet _ v vs s) = freeVars vs `S.union` (freeVars s `underPat` freeVarsPat (TH.VarP v))
    freeVars (SAssign v vs) = freeVars vs
    freeVars (SYield e) = freeVars e
    freeVars (SRet vs) = freeVars vs
    freeVars (SFun fs s) = freeVars s `S.union` freeVarsFunMap fs
    freeVars (SBlock ss) = freeVars ss
    freeVars (SIf e s1 s2) = freeVars e `S.union` freeVars s1 `S.union` freeVars s2
    freeVars (SCase e cs) = freeVars e `S.union` S.unions (flip map cs $ \(p, s) -> freeVars s `underPat` freeVarsPat p)
    freeVars (SNop) = S.empty

instance FreeVars a => FreeVars [a] where
    freeVars ss = S.unions $ map freeVars ss

instance FreeVarsPat a => FreeVarsPat [a] where
    freeVarsPat ss = patUnions $ map freeVarsPat ss

substName :: M.Map TH.Name TH.Exp -> TH.Name -> TH.Exp
substName s n | Just e <- M.lookup n s = e
              | otherwise = TH.VarE n

substExp :: M.Map TH.Name TH.Exp -> TH.Exp -> TH.Exp
substExp s e@(TH.VarE v) = substName s v
substExp _ e@(TH.ConE _) = e
substExp _ e@(TH.LitE _) = e
substExp s   (TH.AppE e1 e2) = TH.AppE (substExp s e1) (substExp s e2)
substExp s   (TH.AppTypeE e t) = TH.AppTypeE (substExp s e) t
substExp s   (TH.InfixE me1 e me2) = TH.InfixE (substExp s <$> me1) (substExp s e) (substExp s <$> me2)
substExp s   (TH.UInfixE e1 e e2) = TH.UInfixE (substExp s e1) (substExp s e) (substExp s e2)
substExp s   (TH.ParensE e) = TH.ParensE (substExp s e)
substExp s   (TH.LamE ps e) = TH.LamE (substPat s <$> ps) (substExp s' e)
    where s' = cutSubst (patUnions $ map freeVarsPat ps) s
substExp s   (TH.TupE es) = TH.TupE (fmap (substExp s) <$> es)
substExp s   (TH.CondE e e1 e2) = TH.CondE (substExp s e) (substExp s e1) (substExp s e2)

renameFieldPat :: M.Map TH.Name TH.Exp -> TH.FieldPat -> TH.FieldPat
renameFieldPat s (n, p) = (n, substPat s p)

substPat :: M.Map TH.Name TH.Exp -> TH.Pat -> TH.Pat
substPat s p@(TH.LitP _) = p
substPat s p@(TH.VarP n) = p
substPat s   (TH.TupP ps) = TH.TupP (substPat s <$> ps)
substPat s   (TH.UnboxedTupP ps) = TH.UnboxedTupP (substPat s <$> ps)
substPat s   (TH.ConP n ps) = TH.ConP n (substPat s <$> ps)
substPat s   (TH.InfixP p1 n p2) = TH.InfixP (substPat s p1) n (substPat s p2)
substPat s   (TH.UInfixP p1 n p2) = TH.UInfixP (substPat s p1) n (substPat s p2)
substPat s   (TH.ParensP p) = TH.ParensP (substPat s p)
substPat s   (TH.TildeP p) = TH.TildeP (substPat s p)
substPat s   (TH.BangP p) = TH.BangP (substPat s p)
substPat s   (TH.AsP n p) = TH.AsP n (substPat s p)
substPat s p@(TH.WildP) = p
substPat s   (TH.RecP n fps) = TH.RecP n (renameFieldPat s <$> fps)
substPat s   (TH.ListP ps) = TH.ListP (substPat s <$> ps)
substPat s   (TH.SigP p t) = TH.SigP (substPat s p) t
substPat s   (TH.ViewP e p) = TH.ViewP (substExp s e) (substPat s p)

cutSubst :: PatFV -> M.Map TH.Name a -> M.Map TH.Name a
cutSubst (PatFV vs _) s = M.withoutKeys s vs

substStmt :: M.Map TH.Name TH.Exp -> Stmt -> Stmt
substStmt su   (SLet t v vs s) = SLet t v (substVStmt su' vs) (substStmt su' s)
    where su' = cutSubst (patSingleton v) su
substStmt su   (SAssign v vs) = SAssign n' (substVStmt su vs)
    where (TH.VarE n') = substName su v
substStmt su   (SYield e) = SYield (substExp su e)
substStmt su   (SRet vs) = SRet (substVStmt su vs)
substStmt su   (SFun fs s) = SFun (flip M.map fs $ \(p, s) -> (substPat su p, substStmt (cutSubst (freeVarsPat p) su) s)) (substStmt su s)
substStmt su   (SBlock ss) = SBlock (substStmt su <$> ss)
substStmt su   (SIf e st sf) = SIf (substExp su e) (substStmt su st) (substStmt su sf)
substStmt su   (SCase e cs) = SCase (substExp su e) (flip map cs $ \(p, s) -> (substPat su p, substStmt (cutSubst (freeVarsPat p) su) s))
substStmt su s@(SNop) = s

substStmtSingle :: TH.Name -> TH.Exp -> Stmt -> Stmt
substStmtSingle n e = substStmt (M.singleton n e)

substVStmt :: M.Map TH.Name TH.Exp -> VStmt -> VStmt
substVStmt su (VExp e) = VExp (substExp su e)
substVStmt su (VCall n e) = VCall n (substExp su e)

renameExp :: M.Map TH.Name TH.Name -> TH.Exp -> TH.Exp
renameExp su = substExp (M.map TH.VarE su)

renamePat :: M.Map TH.Name TH.Name -> TH.Pat -> TH.Pat
renamePat su = substPat (M.map TH.VarE su)

renameStmt :: M.Map TH.Name TH.Name -> Stmt -> Stmt
renameStmt su = substStmt (M.map TH.VarE su)

renameVStmt :: M.Map TH.Name TH.Name -> VStmt -> VStmt
renameVStmt su = substVStmt (M.map TH.VarE su)

renameStmtSingle :: TH.Name -> TH.Name -> Stmt -> Stmt
renameStmtSingle n n' = substStmtSingle n (TH.VarE n')

isConstantExpr :: TH.Exp -> Bool
isConstantExpr (TH.VarE _) = False
isConstantExpr (TH.ConE _) = True
isConstantExpr (TH.LitE _) = True
isConstantExpr (TH.AppE e1 e2) = isConstantExpr e1 && isConstantExpr e2
isConstantExpr (TH.AppTypeE e _) = isConstantExpr e
isConstantExpr (TH.InfixE me1 e me2) = maybe True isConstantExpr me1 && isConstantExpr e && maybe True isConstantExpr me2
isConstantExpr (TH.UInfixE e1 e2 e3) = isConstantExpr e1 && isConstantExpr e2 && isConstantExpr e3
isConstantExpr (TH.ParensE e) = isConstantExpr e
isConstantExpr (TH.LamE _ _) = True
isConstantExpr (TH.LamCaseE _) = True
isConstantExpr (TH.TupE mes) = all (maybe True isConstantExpr) mes
isConstantExpr (TH.UnboxedTupE mes) = all (maybe True isConstantExpr) mes
isConstantExpr (TH.UnboxedSumE e _ _) = isConstantExpr e
isConstantExpr (TH.CondE _ _ _) = False
isConstantExpr (TH.MultiIfE _) = False
isConstantExpr (TH.LetE _ _) = False
isConstantExpr (TH.CaseE _ _) = False
isConstantExpr (TH.DoE _) = False
isConstantExpr (TH.MDoE _) = False
isConstantExpr (TH.CompE _) = False
isConstantExpr (TH.ArithSeqE r) = isConstantRange r
isConstantExpr (TH.ListE es) = all isConstantExpr es
isConstantExpr (TH.SigE e _) = isConstantExpr e
isConstantExpr (TH.RecConE _ res) = all (isConstantExpr . snd) res
isConstantExpr (TH.RecUpdE _ _) = False
isConstantExpr (TH.StaticE _) = False
isConstantExpr (TH.UnboundVarE _) = False
isConstantExpr (TH.LabelE _) = False
isConstantExpr (TH.ImplicitParamVarE _) = False

isConstantRange :: TH.Range -> Bool
isConstantRange (TH.FromR e) = isConstantExpr e
isConstantRange (TH.FromThenR e1 e2) = isConstantExpr e1 && isConstantExpr e2
isConstantRange (TH.FromToR e1 e2) = isConstantExpr e1 && isConstantExpr e2
isConstantRange (TH.FromThenToR e1 e2 e3) = isConstantExpr e1 && isConstantExpr e2 && isConstantExpr e3

