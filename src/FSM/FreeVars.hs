module FSM.FreeVars where

import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Prelude
import FSM.Lang
import Control.Arrow
import qualified FSM.Util.SetClass as SC

class FreeVars a where
    freeVars :: SC.SetClass s => a -> s TH.Name

class FreeVarsPat a where
    freeVarsPat :: SC.SetClass s => a -> PatFV s

data PatFV s = PatFV { patBound :: s TH.Name, patFree :: s TH.Name }

instance SC.SetClass s => Semigroup (PatFV s) where
    PatFV s1 s2 <> PatFV t1 t2 = PatFV (s1 `SC.union` t1) (s2 `SC.union` t2)

instance SC.SetClass s => Monoid (PatFV s) where
    mempty = PatFV mempty mempty

patSingleton :: SC.SetClass s => TH.Name -> PatFV s
patSingleton n = PatFV (SC.singleton n) mempty

patFreeSingleton :: SC.SetClass s => TH.Name -> PatFV s
patFreeSingleton n = PatFV mempty (SC.singleton n)

patFreeVars :: (SC.SetClass s, FreeVars a) => a -> PatFV s
patFreeVars e = PatFV mempty (freeVars e)

underPat :: SC.SetClass s => s TH.Name -> PatFV s -> s TH.Name
underPat s (PatFV bs fs) = fs <> (s `SC.difference` bs)

underPatFV :: SC.SetClass s => PatFV s -> PatFV s -> PatFV s
underPatFV (PatFV bs1 fs1) (PatFV bs2 fs2) = PatFV (bs1 <> bs2) (fs2 <> (fs1 `SC.difference` bs2))

freeVarsUnderPat :: (SC.SetClass s, FreeVarsPat a) => s TH.Name -> a -> s TH.Name
freeVarsUnderPat s p = s `underPat` freeVarsPat p

boundVars :: (SC.SetClass s, FreeVarsPat a) => a -> s TH.Name
boundVars = patBound . freeVarsPat

instance FreeVars a => FreeVars (Maybe a) where
    freeVars = maybe mempty id . fmap freeVars

instance FreeVarsPat a => FreeVarsPat (Maybe a) where
    freeVarsPat = maybe mempty id . fmap freeVarsPat

freeVarsFunMap :: (IsDesugared l, SC.SetClass s) => FunMap l -> s TH.Name
freeVarsFunMap = mconcat . map (\(_, (p, s)) -> freeVars s `freeVarsUnderPat` p) . M.toList

freeVarsGuardedExp :: SC.SetClass s => (TH.Guard, TH.Exp) -> s TH.Name
freeVarsGuardedExp (g, e) = freeVars e `freeVarsUnderPat` g

freeVarsStmts :: SC.SetClass s => [TH.Stmt] -> s TH.Name
freeVarsStmts = patFree . freeVarsPatStmts

freeVarsPatStmts :: SC.SetClass s => [TH.Stmt] -> PatFV s
freeVarsPatStmts [] = mempty
freeVarsPatStmts (s:ss) = freeVarsPatStmts ss `underPatFV` freeVarsPat s

instance FreeVars TH.Type where
    freeVars = const mempty

instance FreeVars TH.Exp where
    freeVars (TH.VarE v) = SC.singleton v
    freeVars (TH.ConE _) = mempty
    freeVars (TH.LitE _) = mempty
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
    freeVars (TH.UnboundVarE _) = mempty
    freeVars (TH.LabelE _) = mempty
    freeVars (TH.ImplicitParamVarE _) = mempty

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
    freeVarsPat (TH.FunD n cs) = patFreeVars cs `underPatFV` patSingleton n

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

instance IsDesugared l => FreeVars (Stmt l) where
    freeVars (SLet _ v vs s) = freeVars vs <> (freeVars s `freeVarsUnderPat` (TH.VarP v))
    freeVars (SAssign _ vs) = freeVars vs
    freeVars (SYield e) = freeVars e
    freeVars (SRet vs) = freeVars vs
    freeVars (SFun fs s) = freeVars s <> freeVarsFunMap fs
    freeVars (SBlock ss) = freeVars ss
    freeVars (SIf e s1 s2) = freeVars e <> freeVars s1 <> freeVars s2
    freeVars (SCase e cs) = freeVars e <> mconcat (flip map cs $ \(p, s) -> freeVars s `freeVarsUnderPat` p)
    freeVars (SNop) = mempty

instance IsDesugared l => FreeVars (Prog l) where
    freeVars prog = freeVars (progBody prog) `SC.difference` boundVars (progInputs prog) `SC.difference` boundVars (progParams prog)

instance FreeVars a => FreeVars [a] where
    freeVars ss = mconcat $ map freeVars ss

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
    subst s   (TH.InfixE me1 e me2) = TH.InfixE (subst s me1) (subst s e) (subst s me2)
    subst s   (TH.UInfixE e1 e e2) = TH.UInfixE (subst s e1) (subst s e) (subst s e2)
    subst s   (TH.ParensE e) = TH.ParensE (subst s e)
    subst s   (TH.LamE ps e) = TH.LamE (subst s ps) (subst (cutSubstPat ps s) e)
    subst s   (TH.LamCaseE ms) = TH.LamCaseE (subst s ms)
    subst s   (TH.TupE es) = TH.TupE (subst s es)
    subst s   (TH.UnboxedTupE es) = TH.UnboxedTupE (subst s es)
    subst s   (TH.UnboxedSumE e al ar) = TH.UnboxedSumE (subst s e) al ar
    subst s   (TH.CondE e e1 e2) = TH.CondE (subst s e) (subst s e1) (subst s e2)
    subst s   (TH.MultiIfE ges) = TH.MultiIfE $ map (\(g, e) -> (subst s g, subst (cutSubstPat g s) e)) ges
    subst s   (TH.LetE ds e) = TH.LetE (subst s ds) (subst (cutSubstPat ds s) e)
    subst s   (TH.CaseE e ms) = TH.CaseE (subst s e) (subst s ms) 
    subst s   (TH.DoE ss) = TH.DoE $ substStmts s ss
    subst s   (TH.MDoE ss) = TH.MDoE $ substStmts s ss
    subst s   (TH.CompE ss) = TH.CompE $ substStmts s ss
    subst s   (TH.ArithSeqE r) = TH.ArithSeqE $ subst s r
    subst s   (TH.ListE es) = TH.ListE $ map (subst s) es
    subst s   (TH.SigE e t) = TH.SigE (subst s e) t
    subst s   (TH.RecConE n fes) = TH.RecConE n (map (id *** subst s) fes)
    subst s   (TH.RecUpdE e fes) = TH.RecUpdE (subst s e) (map (id *** subst s) fes)
    subst s   (TH.StaticE e) = TH.StaticE (subst s e)
    subst _ e@(TH.UnboundVarE _) = e
    subst _ e@(TH.LabelE _) = e
    subst _ e@(TH.ImplicitParamVarE _) = e

instance Subst TH.Range where
    subst s (TH.FromR e) = TH.FromR (subst s e)
    subst s (TH.FromThenR e1 e2) = TH.FromThenR (subst s e1) (subst s e2)
    subst s (TH.FromToR e1 e2) = TH.FromToR (subst s e1) (subst s e2)
    subst s (TH.FromThenToR e1 e2 e3) = TH.FromThenToR (subst s e1) (subst s e2) (subst s e3)

instance Subst TH.Pat where
    subst _ p@(TH.LitP _) = p
    subst _ p@(TH.VarP _) = p
    subst s   (TH.TupP ps) = TH.TupP (subst s ps)
    subst s   (TH.UnboxedTupP ps) = TH.UnboxedTupP (subst s ps)
    subst s   (TH.UnboxedSumP p al ar) = TH.UnboxedSumP (subst s p) al ar
    subst s   (TH.ConP n ps) = TH.ConP n (subst s ps)
    subst s   (TH.InfixP p1 n p2) = TH.InfixP (subst s p1) n (subst s p2)
    subst s   (TH.UInfixP p1 n p2) = TH.UInfixP (subst s p1) n (subst s p2)
    subst s   (TH.ParensP p) = TH.ParensP (subst s p)
    subst s   (TH.TildeP p) = TH.TildeP (subst s p)
    subst s   (TH.BangP p) = TH.BangP (subst s p)
    subst s   (TH.AsP n p) = TH.AsP n (subst s p)
    subst _ p@(TH.WildP) = p
    subst s   (TH.RecP n fps) = TH.RecP n ((id *** subst s) <$> fps)
    subst s   (TH.ListP ps) = TH.ListP (subst s ps)
    subst s   (TH.SigP p t) = TH.SigP (subst s p) t
    subst s   (TH.ViewP e p) = TH.ViewP (subst s e) (subst s p)

instance Subst TH.Match where
    subst s (TH.Match p b ds) = TH.Match (subst s p) (subst s'' b) (subst s' ds)
        where s' = cutSubstPat p s
              s'' = cutSubstPat ds s'

instance Subst TH.Clause where
    subst s (TH.Clause ps b ds) = TH.Clause (subst s ps) (subst s'' b) (subst s' ds)
        where s' = cutSubstPat ps s
              s'' = cutSubstPat ds s'

instance Subst TH.Guard where
    subst s (TH.NormalG e) = TH.NormalG (subst s e)
    subst s (TH.PatG ss) = TH.PatG (substStmts s ss)

instance Subst TH.Body where
    subst s (TH.NormalB e) = TH.NormalB (subst s e)
    subst s (TH.GuardedB ges) = TH.GuardedB (map (substGuardedExp s) ges)

instance Subst TH.Stmt where
    subst s (TH.BindS p e) = TH.BindS (subst s p) (subst s e)
    subst s (TH.LetS ds) = TH.LetS $ subst s ds
    subst s (TH.NoBindS e) = TH.NoBindS $ subst s e
    subst s (TH.ParS sss) = TH.ParS $ map (substStmts s) sss
    subst s (TH.RecS ss) = TH.RecS $ substStmts s ss

instance Subst TH.Dec where
    subst s (TH.ValD p b ds) = TH.ValD (subst s p) (subst s'' b) (subst s' ds)
        where s' = cutSubstPat p s
              s'' = cutSubstPat ds s'
    subst s (TH.FunD n cs) = TH.FunD n (subst (cutSubst (patSingleton n) s) cs)

substStmts :: M.Map TH.Name TH.Exp -> [TH.Stmt] -> [TH.Stmt]
substStmts _  [] = []
substStmts su (s:ss) = subst su s:substStmts (cutSubstPat s su) ss

substGuardedExp :: M.Map TH.Name TH.Exp -> (TH.Guard, TH.Exp) -> (TH.Guard, TH.Exp)
substGuardedExp s (g, e) = (subst s g, subst (cutSubstPat g s) e)

cutSubst :: PatFV S.Set -> M.Map TH.Name a -> M.Map TH.Name a
cutSubst (PatFV vs _) s = M.withoutKeys s vs

cutSubstPat :: FreeVarsPat a => a -> M.Map TH.Name b -> M.Map TH.Name b
cutSubstPat p = cutSubst (freeVarsPat p)

instance IsDesugared l => Subst (Stmt l) where
    subst su   (SLet t v vs s) = SLet t v (subst su' vs) (subst su' s)
        where su' = cutSubst (patSingleton v) su
    subst su   (SAssign v vs) = SAssign n' (subst su vs)
        where (TH.VarE n') = substName su v
    subst su   (SYield e) = SYield (subst su e)
    subst su   (SRet vs) = SRet (subst su vs)
    subst su   (SFun fs s) = SFun (flip M.map fs $ \(p, s') -> (subst su p, subst (cutSubstPat p su) s')) (subst su s)
    subst su   (SBlock ss) = SBlock (subst su <$> ss)
    subst su   (SIf e st sf) = SIf (subst su e) (subst su st) (subst su sf)
    subst su   (SCase e cs) = SCase (subst su e) (flip map cs $ \(p, s) -> (subst su p, subst (cutSubstPat p su) s))
    subst _  s@(SNop) = s

instance Subst a => Subst [a] where
    subst s = map (subst s)

instance Subst a => Subst (Maybe a) where
    subst s = fmap (subst s)

substSingle :: Subst a => TH.Name -> TH.Exp -> a -> a
substSingle n e = subst (M.singleton n e)

instance Subst VStmt where
    subst su (VExp e) = VExp (subst su e)
    subst su (VCall n e) = VCall n (subst su e)

rename :: Subst a => M.Map TH.Name TH.Name -> a -> a
rename su = subst (M.map TH.VarE su)

renameSingle :: Subst a => TH.Name -> TH.Name -> a -> a
renameSingle n n' = substSingle n (TH.VarE n')

boundAsVars :: TH.Pat -> S.Set TH.Name
boundAsVars (TH.LitP _) = mempty
boundAsVars (TH.VarP _) = mempty
boundAsVars (TH.TupP ps) = mconcat $ map boundAsVars ps
boundAsVars (TH.UnboxedTupP ps) = mconcat $ map boundAsVars ps
boundAsVars (TH.UnboxedSumP p _ _) = boundAsVars p
boundAsVars (TH.ConP _ ps) = mconcat $ map boundAsVars ps
boundAsVars (TH.InfixP p1 _ p2) = boundAsVars p1 <> boundAsVars p2
boundAsVars (TH.UInfixP p1 _ p2) = boundAsVars p1 <> boundAsVars p2
boundAsVars (TH.ParensP p) = boundAsVars p
boundAsVars (TH.TildeP p) = boundAsVars p
boundAsVars (TH.BangP p) = boundAsVars p
boundAsVars (TH.AsP n p) = boundAsVars p <> S.singleton n
boundAsVars (TH.WildP) = mempty
boundAsVars (TH.RecP _ fps) = mconcat $ map (boundAsVars . snd) fps
boundAsVars (TH.ListP ps) = mconcat $ map boundAsVars ps
boundAsVars (TH.SigP p _) = boundAsVars p
boundAsVars (TH.ViewP _ p) = boundAsVars p

substPat :: M.Map TH.Name TH.Pat -> TH.Pat -> TH.Pat
substPat _ p@(TH.LitP _) = p
substPat s p@(TH.VarP n) | Just p' <- M.lookup n s = p'
                         | otherwise = p
substPat s   (TH.TupP ps) = TH.TupP (map (substPat s) ps)
substPat s   (TH.UnboxedTupP ps) = TH.UnboxedTupP (map (substPat s) ps)
substPat s   (TH.UnboxedSumP p al ar) = TH.UnboxedSumP (substPat s p) al ar
substPat s   (TH.ConP n ps) = TH.ConP n (map (substPat s) ps)
substPat s   (TH.InfixP p1 n p2) = TH.InfixP (substPat s p1) n (substPat s p2)
substPat s   (TH.UInfixP p1 n p2) = TH.UInfixP (substPat s p1) n (substPat s p2)
substPat s   (TH.ParensP p) = TH.ParensP (substPat s p)
substPat s   (TH.TildeP p) = TH.TildeP (substPat s p)
substPat s   (TH.BangP p) = TH.BangP (substPat s p)
substPat s   (TH.AsP n p) | Just p' <- M.lookup n s = let TH.VarP n' = p' in TH.AsP n' (substPat s p)
                          | otherwise = TH.AsP n (substPat s p)
substPat _ p@(TH.WildP) = p
substPat s   (TH.RecP n fps) = TH.RecP n ((id *** substPat s) <$> fps)
substPat s   (TH.ListP ps) = TH.ListP (map (substPat s) ps)
substPat s   (TH.SigP p t) = TH.SigP (substPat s p) t
substPat s   (TH.ViewP e p) = TH.ViewP e (substPat s p)

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

