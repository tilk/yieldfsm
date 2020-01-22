module THFreeVars where

import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Prelude
import Data.Maybe(maybe)

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
freeVarsExp (TH.TupE es) = S.unions $ map freeVarsExp es
freeVarsExp (TH.UnboxedTupE es) = S.unions $ map freeVarsExp es
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
freeVarsFieldPat (n, p) = freeVarsPat p `patUnion` patSingleton n

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

substExp :: M.Map TH.Name TH.Exp -> TH.Exp -> TH.Exp
substExp s e@(TH.VarE v) | Just ve <- M.lookup v s = ve
                         | otherwise = e
substExp s e@(TH.ConE _) = e
substExp s e@(TH.LitE _) = e
substExp s   (TH.AppE e1 e2) = TH.AppE (substExp s e1) (substExp s e2)
substExp s   (TH.AppTypeE e t) = TH.AppTypeE (substExp s e) t
substExp s   (TH.InfixE me1 e me2) = TH.InfixE (substExp s <$> me1) (substExp s e) (substExp s <$> me2)
substExp s   (TH.UInfixE e1 e e2) = TH.UInfixE (substExp s e1) (substExp s e) (substExp s e2)
substExp s   (TH.ParensE e) = TH.ParensE (substExp s e)
substExp s   (TH.LamE ps e) = TH.LamE (substPat s <$> ps) (substExp (cutSubst (patUnions $ map freeVarsPat ps) s) e)

substFieldPat :: M.Map TH.Name TH.Exp -> TH.FieldPat -> TH.FieldPat
substFieldPat s (n, p) = (n, substPat s p)

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
substPat s   (TH.RecP n fps) = TH.RecP n (substFieldPat s <$> fps)
substPat s   (TH.ListP ps) = TH.ListP (substPat s <$> ps)
substPat s   (TH.SigP p t) = TH.SigP (substPat s p) t
substPat s   (TH.ViewP e p) = TH.ViewP (substExp s e) (substPat s p)

cutSubst :: PatFV -> M.Map TH.Name TH.Exp -> M.Map TH.Name TH.Exp
cutSubst (PatFV vs _) s = M.withoutKeys s vs

