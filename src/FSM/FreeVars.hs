module FSM.FreeVars where

import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Prelude
import FSM.Lang

class FreeVars a where
    freeVars :: a -> S.Set TH.Name

class FreeVarsPat a where
    freeVarsPat :: a -> PatFV

data PatFV = PatFV { patBound :: S.Set TH.Name, patFree :: S.Set TH.Name }

instance Semigroup PatFV where
    PatFV s1 s2 <> PatFV t1 t2 = PatFV (s1 <> t1) (s2 <> t2)

instance Monoid PatFV where
    mempty = PatFV S.empty S.empty

patSingleton :: TH.Name -> PatFV
patSingleton n = PatFV (S.singleton n) S.empty

patFreeVars :: FreeVars a => a -> PatFV
patFreeVars e = PatFV S.empty (freeVars e)

underPat :: S.Set TH.Name -> PatFV -> S.Set TH.Name
underPat s (PatFV bs fs) = fs <> (s `S.difference` bs)

underPatFV :: PatFV -> PatFV -> PatFV
underPatFV (PatFV bs1 fs1) (PatFV bs2 fs2) = PatFV (bs1 <> bs2) (fs2 <> (fs1 `S.difference` bs2))

freeVarsUnderPat :: FreeVarsPat a => S.Set TH.Name -> a -> S.Set TH.Name
freeVarsUnderPat s p = s `underPat` freeVarsPat p

boundVars :: FreeVarsPat a => a -> S.Set TH.Name
boundVars = patBound . freeVarsPat

instance FreeVars a => FreeVars (Maybe a) where
    freeVars = maybe S.empty id . fmap freeVars

instance FreeVarsPat a => FreeVarsPat (Maybe a) where
    freeVarsPat = maybe mempty id . fmap freeVarsPat

freeVarsFunMap :: FunMap -> S.Set TH.Name
freeVarsFunMap = S.unions . map (\(_, (p, s)) -> freeVars s `freeVarsUnderPat` p) . M.toList

freeVarsGuardedExp :: (TH.Guard, TH.Exp) -> S.Set TH.Name
freeVarsGuardedExp (g, e) = freeVars e `freeVarsUnderPat` g

freeVarsStmts :: [TH.Stmt] -> S.Set TH.Name
freeVarsStmts = patFree . freeVarsPatStmts

freeVarsPatStmts :: [TH.Stmt] -> PatFV
freeVarsPatStmts [] = mempty
freeVarsPatStmts (s:ss) = freeVarsPatStmts ss `underPatFV` freeVarsPat s

instance FreeVars TH.Type where
    freeVars = const S.empty

instance FreeVars TH.Exp where
    freeVars (TH.VarE v) = S.singleton v
    freeVars (TH.ConE _) = S.empty
    freeVars (TH.LitE _) = S.empty
    freeVars (TH.AppE e1 e2) = freeVars e1 <> freeVars e2
    freeVars (TH.AppTypeE e _ ) = freeVars e
    freeVars (TH.InfixE me1 e me2) = freeVars e <> freeVars me1 <> freeVars me2
    freeVars (TH.UInfixE e1 e e2) = freeVars e <> freeVars e1 <> freeVars e2
    freeVars (TH.ParensE e) = freeVars e
    freeVars (TH.LamE ps e) = freeVars e `underPat` mconcat (map freeVarsPat ps)
    freeVars (TH.LamCaseE ms) = freeVars ms
    freeVars (TH.TupE es) = freeVars es
    freeVars (TH.UnboxedTupE es) = freeVars es
    freeVars (TH.UnboxedSumE e _ _) = freeVars e
    freeVars (TH.CondE e e1 e2) = freeVars e <> freeVars e1 <> freeVars e2
    freeVars (TH.MultiIfE ges) = mconcat $ map freeVarsGuardedExp ges
    freeVars (TH.LetE ds e) = freeVars e `freeVarsUnderPat` ds
    freeVars (TH.CaseE e ms) = freeVars e <> freeVars ms
    freeVars (TH.DoE ss) = freeVarsStmts ss
    freeVars (TH.MDoE ss) = freeVarsStmts ss
    freeVars (TH.CompE ss) = freeVarsStmts ss
    freeVars (TH.ArithSeqE r) = freeVars r
    freeVars (TH.ListE es) = freeVars es
    freeVars (TH.SigE e _) = freeVars e
    freeVars (TH.RecConE _ fes) = mconcat $ map (freeVars . snd) fes
    freeVars (TH.RecUpdE e fes) = freeVars e <> (mconcat $ map (freeVars . snd) fes)
    freeVars (TH.StaticE e) = freeVars e
    freeVars (TH.UnboundVarE _) = S.empty
    freeVars (TH.LabelE _) = S.empty
    freeVars (TH.ImplicitParamVarE _) = S.empty

instance FreeVars TH.Range where
    freeVars (TH.FromR e) = freeVars e
    freeVars (TH.FromThenR e1 e2) = freeVars e1 <> freeVars e2
    freeVars (TH.FromToR e1 e2) = freeVars e1 <> freeVars e2
    freeVars (TH.FromThenToR e1 e2 e3) = freeVars e1 <> freeVars e2 <> freeVars e3

instance FreeVars TH.Clause where
    freeVars (TH.Clause ps b ds) = freeVars b `underPat` (freeVarsPat ds `underPatFV` freeVarsPat ps)

instance FreeVarsPat TH.Pat where
    freeVarsPat (TH.LitP _) = mempty
    freeVarsPat (TH.VarP n) = patSingleton n
    freeVarsPat (TH.TupP ps) = freeVarsPat ps
    freeVarsPat (TH.UnboxedTupP ps) = freeVarsPat ps
    freeVarsPat (TH.UnboxedSumP p _ _) = freeVarsPat p
    freeVarsPat (TH.ConP _ ps) = freeVarsPat ps
    freeVarsPat (TH.InfixP p1 _ p2) = freeVarsPat p1 <> freeVarsPat p2
    freeVarsPat (TH.UInfixP p1 _ p2) = freeVarsPat p1 <> freeVarsPat p2
    freeVarsPat (TH.ParensP p) = freeVarsPat p
    freeVarsPat (TH.TildeP p) = freeVarsPat p
    freeVarsPat (TH.BangP p) = freeVarsPat p
    freeVarsPat (TH.AsP n p) = freeVarsPat p <> patSingleton n
    freeVarsPat (TH.WildP) = mempty
    freeVarsPat (TH.RecP _ fps) = mconcat $ map (freeVarsPat . snd) fps
    freeVarsPat (TH.ListP ps) = freeVarsPat ps
    freeVarsPat (TH.SigP p _) = freeVarsPat p
    freeVarsPat (TH.ViewP e p) = freeVarsPat p <> patFreeVars e

instance FreeVarsPat TH.Stmt where
    freeVarsPat (TH.BindS p e) = freeVarsPat p <> patFreeVars e
    freeVarsPat (TH.LetS ds) = mconcat $ map freeVarsPat ds
    freeVarsPat (TH.NoBindS e) = patFreeVars e
    freeVarsPat (TH.ParS sss) = mconcat $ map freeVarsPatStmts sss
    freeVarsPat (TH.RecS ss) = freeVarsPatStmts ss

instance FreeVarsPat TH.Dec where
    freeVarsPat (TH.ValD p b ds) = patFreeVars b `underPatFV` (freeVarsPat ds `underPatFV` freeVarsPat p)
    freeVarsPat (TH.FunD n cs) = patSingleton n <> patFreeVars cs

instance FreeVars TH.Pat where
    freeVars = patFree . freeVarsPat

instance FreeVars TH.Stmt where
    freeVars = patFree . freeVarsPat

instance FreeVars TH.Body where
    freeVars (TH.NormalB e) = freeVars e
    freeVars (TH.GuardedB ges) = mconcat $ map freeVarsGuardedExp ges

instance FreeVarsPat TH.Guard where
    freeVarsPat (TH.NormalG e) = patFreeVars e
    freeVarsPat (TH.PatG ss) = freeVarsPatStmts ss

instance FreeVars TH.Match where
    freeVars (TH.Match p b ds) = freeVars b `underPat` (freeVarsPat ds `underPatFV` freeVarsPat p)

instance FreeVars VStmt where
    freeVars (VExp e) = freeVars e
    freeVars (VCall _ e) = freeVars e

instance FreeVars Stmt where
    freeVars (SLet _ v vs s) = freeVars vs <> (freeVars s `freeVarsUnderPat` (TH.VarP v))
    freeVars (SAssign _ vs) = freeVars vs
    freeVars (SYield e) = freeVars e
    freeVars (SRet vs) = freeVars vs
    freeVars (SFun fs s) = freeVars s <> freeVarsFunMap fs
    freeVars (SBlock ss) = freeVars ss
    freeVars (SIf e s1 s2) = freeVars e <> freeVars s1 <> freeVars s2
    freeVars (SCase e cs) = freeVars e <> S.unions (flip map cs $ \(p, s) -> freeVars s `freeVarsUnderPat` p)
    freeVars (SNop) = S.empty

instance FreeVars a => FreeVars [a] where
    freeVars ss = S.unions $ map freeVars ss

instance FreeVarsPat a => FreeVarsPat [a] where
    freeVarsPat ss = mconcat $ map freeVarsPat ss

substName :: M.Map TH.Name TH.Exp -> TH.Name -> TH.Exp
substName s n | Just e <- M.lookup n s = e
              | otherwise = TH.VarE n

class Subst a where
    subst :: M.Map TH.Name TH.Exp -> a -> a

instance Subst TH.Exp where
    subst s   (TH.VarE v) = substName s v
    subst _ e@(TH.ConE _) = e
    subst _ e@(TH.LitE _) = e
    subst s   (TH.AppE e1 e2) = TH.AppE (subst s e1) (subst s e2)
    subst s   (TH.AppTypeE e t) = TH.AppTypeE (subst s e) t
    subst s   (TH.InfixE me1 e me2) = TH.InfixE (subst s <$> me1) (subst s e) (subst s <$> me2)
    subst s   (TH.UInfixE e1 e e2) = TH.UInfixE (subst s e1) (subst s e) (subst s e2)
    subst s   (TH.ParensE e) = TH.ParensE (subst s e)
    subst s   (TH.LamE ps e) = TH.LamE (subst s <$> ps) (subst s' e)
        where s' = cutSubst (mconcat $ map freeVarsPat ps) s
    subst s   (TH.TupE es) = TH.TupE (fmap (subst s) <$> es)
    subst s   (TH.CondE e e1 e2) = TH.CondE (subst s e) (subst s e1) (subst s e2)

renameFieldPat :: M.Map TH.Name TH.Exp -> TH.FieldPat -> TH.FieldPat
renameFieldPat s (n, p) = (n, subst s p)

instance Subst TH.Pat where
    subst _ p@(TH.LitP _) = p
    subst _ p@(TH.VarP _) = p
    subst s   (TH.TupP ps) = TH.TupP (subst s <$> ps)
    subst s   (TH.UnboxedTupP ps) = TH.UnboxedTupP (subst s <$> ps)
    subst s   (TH.ConP n ps) = TH.ConP n (subst s <$> ps)
    subst s   (TH.InfixP p1 n p2) = TH.InfixP (subst s p1) n (subst s p2)
    subst s   (TH.UInfixP p1 n p2) = TH.UInfixP (subst s p1) n (subst s p2)
    subst s   (TH.ParensP p) = TH.ParensP (subst s p)
    subst s   (TH.TildeP p) = TH.TildeP (subst s p)
    subst s   (TH.BangP p) = TH.BangP (subst s p)
    subst s   (TH.AsP n p) = TH.AsP n (subst s p)
    subst _ p@(TH.WildP) = p
    subst s   (TH.RecP n fps) = TH.RecP n (renameFieldPat s <$> fps)
    subst s   (TH.ListP ps) = TH.ListP (subst s <$> ps)
    subst s   (TH.SigP p t) = TH.SigP (subst s p) t
    subst s   (TH.ViewP e p) = TH.ViewP (subst s e) (subst s p)

cutSubst :: PatFV -> M.Map TH.Name a -> M.Map TH.Name a
cutSubst (PatFV vs _) s = M.withoutKeys s vs

instance Subst Stmt where
    subst su   (SLet t v vs s) = SLet t v (subst su' vs) (subst su' s)
        where su' = cutSubst (patSingleton v) su
    subst su   (SAssign v vs) = SAssign n' (subst su vs)
        where (TH.VarE n') = substName su v
    subst su   (SYield e) = SYield (subst su e)
    subst su   (SRet vs) = SRet (subst su vs)
    subst su   (SFun fs s) = SFun (flip M.map fs $ \(p, s') -> (subst su p, subst (cutSubst (freeVarsPat p) su) s')) (subst su s)
    subst su   (SBlock ss) = SBlock (subst su <$> ss)
    subst su   (SIf e st sf) = SIf (subst su e) (subst su st) (subst su sf)
    subst su   (SCase e cs) = SCase (subst su e) (flip map cs $ \(p, s) -> (subst su p, subst (cutSubst (freeVarsPat p) su) s))
    subst _  s@(SNop) = s

substSingle :: Subst a => TH.Name -> TH.Exp -> a -> a
substSingle n e = subst (M.singleton n e)

instance Subst VStmt where
    subst su (VExp e) = VExp (subst su e)
    subst su (VCall n e) = VCall n (subst su e)

rename :: Subst a => M.Map TH.Name TH.Name -> a -> a
rename su = subst (M.map TH.VarE su)

renameSingle :: Subst a => TH.Name -> TH.Name -> a -> a
renameSingle n n' = substSingle n (TH.VarE n')

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

