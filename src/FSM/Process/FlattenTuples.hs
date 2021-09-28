module FSM.Process.FlattenTuples(flattenTuples) where

import FSM.Lang
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

type PatMap = M.Map TH.Name TH.Pat

canFlattenExp :: TH.Name -> TH.Pat -> TH.Exp -> S.Set TH.Name
canFlattenExp n (TH.TupP ps) (TH.TupE mes)
    | length ps == length mes, all isJust mes = S.unions $ zipWith (canFlattenExp n) ps (map fromJust mes)
    | otherwise = S.empty
canFlattenExp n (TH.TupP _) _ = S.singleton n
canFlattenExp _ _ _ = S.empty

canFlattenVStmt :: PatMap -> VStmt -> S.Set TH.Name
canFlattenVStmt _  (VExp _) = S.empty
canFlattenVStmt ps (VCall n e)
    | Just p <- M.lookup n ps = canFlattenExp n p e
    | otherwise = S.singleton n

canFlattenStmt :: PatMap -> Stmt -> S.Set TH.Name
canFlattenStmt _  SNop = S.empty
canFlattenStmt ps (SLet _ _ vs s) = canFlattenVStmt ps vs `S.union` canFlattenStmt ps s
canFlattenStmt ps (SAssign _ vs) = canFlattenVStmt ps vs
canFlattenStmt _  (SYield _) = S.empty
canFlattenStmt ps (SRet vs) = canFlattenVStmt ps vs
canFlattenStmt ps (SBlock ss) = S.unions $ map (canFlattenStmt ps) ss
canFlattenStmt ps (SIf _ st sf) = canFlattenStmt ps st `S.union` canFlattenStmt ps sf
canFlattenStmt ps (SCase _ cs) = S.unions $ map (canFlattenStmt ps . snd) cs
canFlattenStmt _  (SFun _ _) = error "Not in lambda-lifted form"

canFlattenFunMap :: PatMap -> FunMap -> S.Set TH.Name
canFlattenFunMap ps = S.unions . map (canFlattenStmt ps . snd . snd) . M.toList

flattenExp :: TH.Pat -> TH.Exp -> TH.Exp
flattenExp p@(TH.TupP _) e = tupE $ fl p e
    where
    fl (TH.TupP ps) (TH.TupE mes) = concat $ zipWith fl ps (map fromJust mes)
    fl _ e' = [e']
flattenExp _ _ = error "Non-tuple pattern for flattening"

flattenVStmt :: PatMap -> VStmt -> VStmt
flattenVStmt _  (VExp e) = VExp e
flattenVStmt ps (VCall n e)
    | Just p <- M.lookup n ps = VCall n (flattenExp p e)
    | otherwise = VCall n e

flattenStmt :: PatMap -> Stmt -> Stmt
flattenStmt _  s@(SNop) = s
flattenStmt ps   (SLet t n vs s) = SLet t n (flattenVStmt ps vs) (flattenStmt ps s)
flattenStmt ps   (SAssign n vs) = SAssign n (flattenVStmt ps vs)
flattenStmt _  s@(SYield _) = s
flattenStmt ps   (SRet vs) = SRet $ flattenVStmt ps vs
flattenStmt ps   (SBlock ss) = SBlock $ map (flattenStmt ps) ss
flattenStmt ps   (SIf e st sf) = SIf e (flattenStmt ps st) (flattenStmt ps sf)
flattenStmt ps   (SCase e cs) = SCase e (map (id *** flattenStmt ps) cs)
flattenStmt _    (SFun _ _) = error "Not in lambda-lifted form"

flattenPat :: TH.Pat -> TH.Pat
flattenPat p@(TH.TupP _) = tupP $ fl p
    where
    fl (TH.TupP ps) = concatMap fl ps
    fl p' = [p']
flattenPat _ = error "Non-tuple pattern for flattening"

flattenFun :: PatMap -> TH.Name -> (TH.Pat, Stmt) -> (TH.Pat, Stmt)
flattenFun ps n (p, s) | n `M.member` ps = (flattenPat p, flattenStmt ps s)
                       | otherwise = (p, flattenStmt ps s)

flattenFunMap :: PatMap -> FunMap -> FunMap
flattenFunMap ps fs = M.mapWithKey (flattenFun ps) fs

isTupP :: TH.Pat -> Bool
isTupP (TH.TupP _) = True
isTupP _ = False

flattenTuples :: NProg -> NProg
flattenTuples prog = prog {
        nProgFuns = flattenFunMap flatPat $ nProgFuns prog,
        nProgInitParam = if nProgInit prog `M.member` flatPat then initPar else nProgInitParam prog
    }
    where
    pats = M.filter isTupP $ M.map fst $ nProgFuns prog
    nonFlatSet = canFlattenFunMap pats (nProgFuns prog) `S.union` initSet
    (initSet, initPar) = case M.lookup (nProgInit prog) pats of
        Just p -> (canFlattenExp (nProgInit prog) p (nProgInitParam prog), flattenExp p (nProgInitParam prog))
        Nothing -> (S.singleton (nProgInit prog), nProgInitParam prog)
    flatPat = M.filterWithKey (\n _ -> n `S.notMember` nonFlatSet) pats

