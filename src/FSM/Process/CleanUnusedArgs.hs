module FSM.Process.CleanUnusedArgs(cleanUnusedArgs) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

type UFMap = M.Map TH.Name (Either Bool [Bool])

canCleanVStmt :: UFMap -> VStmt -> S.Set TH.Name
canCleanVStmt _ (VExp _) = S.empty
canCleanVStmt m (VCall n e) = canCleanExp m n e

canCleanStmt :: UFMap -> Stmt -> S.Set TH.Name
canCleanStmt _ SNop = S.empty
canCleanStmt m (SLet _ _ vs s) = canCleanVStmt m vs `S.union` canCleanStmt m s
canCleanStmt _ (SAssign _ _) = S.empty
canCleanStmt _ (SYield _) = S.empty
canCleanStmt m (SRet vs) = canCleanVStmt m vs
canCleanStmt m (SBlock ss) = S.unions $ map (canCleanStmt m) ss
canCleanStmt m (SIf _ st sf) = canCleanStmt m st `S.union` canCleanStmt m sf
canCleanStmt m (SCase _ cs) = S.unions $ map (canCleanStmt m . snd) cs
canCleanStmt _ (SFun _ _) = error "Not in lambda-lifted form"

canCleanFunMap :: UFMap -> FunMap -> S.Set TH.Name
canCleanFunMap m = S.unions . map f . M.toList
    where 
    f (n, (p, s)) = canCleanPat m n p `S.union` canCleanStmt m s

canCleanPat :: UFMap -> TH.Name -> TH.Pat -> S.Set TH.Name
canCleanPat m n p | Just (Right bs) <- r, or bs, TH.TupP ps <- p, length bs == length ps = S.empty
                  | Just (Left True) <- r = S.empty
                  | otherwise = S.singleton n
    where r = M.lookup n m

canCleanExp :: UFMap -> TH.Name -> TH.Exp -> S.Set TH.Name
canCleanExp m n e | Just (Right bs) <- r, or bs, TH.TupE mes <- e, all isJust mes, length bs == length mes = S.empty
                  | Just (Left True) <- r = S.empty
                  | otherwise = S.singleton n
    where r = M.lookup n m

unusedFunMap :: FunMap -> UFMap
unusedFunMap = M.map f where
    f (p, s) | TH.TupP ps <- p = Right $ map g ps
             | otherwise       = Left $ g p
        where
        fvs = freeVars s
        g p' = S.null $ boundVars p' `S.intersection` fvs

doCleanPat :: UFMap -> TH.Name -> TH.Pat -> TH.Pat
doCleanPat m n p | Just (Right bs) <- r, TH.TupP ps <- p = tupP $ map snd $ filter (not . fst) $ zip bs ps
                 | Just (Left b) <- r = if b then tupP [] else p
                 | otherwise = p
    where r = M.lookup n m

doCleanExp :: UFMap -> TH.Name -> TH.Exp -> TH.Exp
doCleanExp m n e | Just (Right bs) <- r, TH.TupE mes <- e = tupE $ map snd $ filter (not . fst) $ zip bs $ map fromJust mes
                 | Just (Left b) <- r = if b then tupE [] else e
                 | otherwise = e
    where r = M.lookup n m

doCleanVStmt :: UFMap -> VStmt -> VStmt
doCleanVStmt _ vs@(VExp _) = vs
doCleanVStmt m (VCall n e) = VCall n (doCleanExp m n e)

doCleanStmt :: UFMap -> Stmt -> Stmt
doCleanStmt _ s@(SNop) = s
doCleanStmt m   (SLet n t vs s) = SLet n t (doCleanVStmt m vs) (doCleanStmt m s)
doCleanStmt _ s@(SAssign _ _) = s
doCleanStmt _ s@(SYield _) = s
doCleanStmt m   (SRet vs) = SRet (doCleanVStmt m vs)
doCleanStmt m   (SBlock ss) = SBlock $ map (doCleanStmt m) ss
doCleanStmt m   (SIf e st sf) = SIf e (doCleanStmt m st) (doCleanStmt m sf)
doCleanStmt m   (SCase e cs) = SCase e $ map (id *** doCleanStmt m) cs
doCleanStmt _   (SFun _ _) = error "Not in lambda-lifted form"

doCleanFunMap :: UFMap -> FunMap -> FunMap
doCleanFunMap m = M.mapWithKey f where
    f n (p, s) = (doCleanPat m n p, doCleanStmt m s)

cleanUnusedArgs :: NProg -> NProg
cleanUnusedArgs prog = prog {
    nProgFuns = doCleanFunMap unuseds (nProgFuns prog),
    nProgInitParam = doCleanExp unuseds (nProgInit prog) (nProgInitParam prog)
}
    where
    nonCleanSet = canCleanFunMap ufm (nProgFuns prog) `S.union` canCleanExp ufm (nProgInit prog) (nProgInitParam prog)
    unuseds = ufm `M.difference` M.fromSet (const []) nonCleanSet
    ufm = unusedFunMap (nProgFuns prog)


