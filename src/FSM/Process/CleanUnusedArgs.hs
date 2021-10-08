module FSM.Process.CleanUnusedArgs(cleanUnusedArgs) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

canCleanVStmt :: VStmt -> S.Set TH.Name
canCleanVStmt (VExp _) = S.empty
canCleanVStmt (VCall n e) = canCleanExp n e

canCleanStmt :: Stmt -> S.Set TH.Name
canCleanStmt SNop = S.empty
canCleanStmt (SLet _ _ vs s) = canCleanVStmt vs `S.union` canCleanStmt s
canCleanStmt (SAssign _ vs) = canCleanVStmt vs
canCleanStmt (SYield _) = S.empty
canCleanStmt (SRet vs) = canCleanVStmt vs
canCleanStmt (SBlock ss) = S.unions $ map canCleanStmt ss
canCleanStmt (SIf _ st sf) = canCleanStmt st `S.union` canCleanStmt sf
canCleanStmt (SCase _ cs) = S.unions $ map (canCleanStmt . snd) cs
canCleanStmt (SFun _ _) = error "Not in lambda-lifted form"

canCleanFunMap :: FunMap -> S.Set TH.Name
canCleanFunMap = S.unions . map f . M.toList
    where 
    f (n, (p, s)) = canCleanPat n p `S.union` canCleanStmt s

canCleanPat :: TH.Name -> TH.Pat -> S.Set TH.Name
canCleanPat _ (TH.TupP _) = S.empty
canCleanPat n _ = S.singleton n

canCleanExp :: TH.Name -> TH.Exp -> S.Set TH.Name
canCleanExp _ (TH.TupE mes) | all isJust mes = S.empty
canCleanExp n _ = S.singleton n

unusedFunMap :: FunMap -> M.Map TH.Name [Bool]
unusedFunMap = M.map f where
    f (TH.TupP ps, s) = map g ps where
        fvs = freeVars s
        g p = S.null $ boundVars p `S.intersection` fvs
    f (_, _) = []

doCleanPat :: M.Map TH.Name [Bool] -> TH.Name -> TH.Pat -> TH.Pat
doCleanPat m n p | Just bs <- M.lookup n m, TH.TupP ps <- p = tupP $ map snd $ filter (not . fst) $ zip bs ps
                 | otherwise = p

doCleanExp :: M.Map TH.Name [Bool] -> TH.Name -> TH.Exp -> TH.Exp
doCleanExp m n e | Just bs <- M.lookup n m, TH.TupE mes <- e = tupE $ map snd $ filter (not . fst) $ zip bs $ map fromJust mes
                 | otherwise = e

doCleanVStmt :: M.Map TH.Name [Bool] -> VStmt -> VStmt
doCleanVStmt _ vs@(VExp _) = vs
doCleanVStmt m (VCall n e) = VCall n (doCleanExp m n e)

doCleanStmt :: M.Map TH.Name [Bool] -> Stmt -> Stmt
doCleanStmt _ s@(SNop) = s
doCleanStmt m   (SLet n t vs s) = SLet n t (doCleanVStmt m vs) (doCleanStmt m s)
doCleanStmt m   (SAssign n vs) = SAssign n (doCleanVStmt m vs)
doCleanStmt _ s@(SYield _) = s
doCleanStmt m   (SRet vs) = SRet (doCleanVStmt m vs)
doCleanStmt m   (SBlock ss) = SBlock $ map (doCleanStmt m) ss
doCleanStmt m   (SIf e st sf) = SIf e (doCleanStmt m st) (doCleanStmt m sf)
doCleanStmt m   (SCase e cs) = SCase e $ map (id *** doCleanStmt m) cs
doCleanStmt _   (SFun _ _) = error "Not in lambda-lifted form"

doCleanFunMap :: M.Map TH.Name [Bool] -> FunMap -> FunMap
doCleanFunMap m = M.mapWithKey f where
    f n (p, s) = (doCleanPat m n p, doCleanStmt m s)

cleanUnusedArgs :: NProg -> NProg
cleanUnusedArgs prog = prog {
    nProgFuns = doCleanFunMap unuseds (nProgFuns prog),
    nProgInitParam = doCleanExp unuseds (nProgInit prog) (nProgInitParam prog)
}
    where
    nonCleanSet = canCleanFunMap (nProgFuns prog) `S.union` canCleanExp (nProgInit prog) (nProgInitParam prog)
    unuseds = unusedFunMap (nProgFuns prog) `M.difference` M.fromSet (const []) nonCleanSet


