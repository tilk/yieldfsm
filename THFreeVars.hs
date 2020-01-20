module THFreeVars where

import qualified Language.Haskell.TH as TH
import qualified Data.Set as S
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



