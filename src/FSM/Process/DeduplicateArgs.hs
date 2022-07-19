{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

This module defines an argument deduplication transformation.
-}
module FSM.Process.DeduplicateArgs(deduplicateArgs) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

type Dups = M.Map TH.Name TH.Name

type FunDups = M.Map TH.Name Dups

dupSubst :: Dups -> M.Map TH.Name TH.Exp
dupSubst = M.map TH.VarE

lookupDup :: TH.Name -> FunDups -> Dups
lookupDup n = maybe M.empty id . M.lookup n

dupRep :: TH.Name -> Dups -> TH.Name
dupRep v ds | Just v' <- M.lookup v ds = v'
            | otherwise = v

intersectDups :: Dups -> Dups -> Dups
intersectDups ds1 ds2 = M.fromList [(v, r v) | v <- S.toList (M.keysSet ds1 `S.union` M.keysSet ds2)]
    where
    r v | dr1 == dr2 = dr1
        | dupRep dr1 ds2 == dr2 = dr1
        | dupRep dr2 ds1 == dr1 = dr2
        | otherwise = v
        where
        dr1 = dupRep v ds1
        dr2 = dupRep v ds2

intersectFunDups :: FunDups -> FunDups -> FunDups
intersectFunDups = M.unionWith intersectDups

intersectsFunDups :: [FunDups] -> FunDups
intersectsFunDups = foldr intersectFunDups M.empty

patVars :: TH.Pat -> [TH.Name]
patVars (TH.TupP ps) = [v | TH.VarP v <- ps]
patVars _ = []

initialDupsVars :: [TH.Name] -> Dups
initialDupsVars [] = M.empty
initialDupsVars (v:vs) = M.fromList [(v', v) | v' <- vs]

initialDupsPat :: TH.Pat -> Dups
initialDupsPat = initialDupsVars . patVars

initialFunDups :: FunMap l -> FunDups
initialFunDups = M.map $ initialDupsPat . fst

isDup :: Dups -> TH.Exp -> TH.Exp -> Bool
isDup ds e e' = subst dss e == subst dss e' where dss = dupSubst ds

newDupsVars :: Dups -> [(TH.Name, TH.Exp)] -> Dups
newDupsVars _ [] = M.empty
newDupsVars ds ((v, e):vs) = M.fromList [(v', v) | (v', e') <- vs, isDup ds e e'] `M.union` newDupsVars ds vs

newDupsCall :: Dups -> TH.Pat -> TH.Exp -> Dups
newDupsCall ds (TH.TupP ps) (TH.TupE mes) | all isJust mes =
    newDupsVars ds [(v, e) | (TH.VarP v, Just e) <- zip ps mes]
newDupsCall _ _ _ = M.empty

newDupsStmt :: IsLowered l => Dups -> FunMap l -> Stmt l -> FunDups
newDupsStmt ds fs (SLet _ _ _ s) = newDupsStmt ds fs s
newDupsStmt ds fs (SYieldT _ s) = newDupsStmt ds fs s
newDupsStmt ds fs (SIf _ st sf) = newDupsStmt ds fs st `intersectFunDups` newDupsStmt ds fs sf
newDupsStmt ds fs (SCase _ cs) = intersectsFunDups $ map (newDupsStmt ds fs . snd) cs
newDupsStmt ds fs (SRet (VCall n e)) | Just (p, _) <- M.lookup n fs = M.singleton n $ newDupsCall ds p e
                                     | otherwise = error "call to undefined function"
newDupsStmt _  _  (SRet (VExp _)) = error "ret exp in lowered"

newDupsFunMap :: IsLowered l => FunMap l -> FunDups -> FunDups
newDupsFunMap fs ds = intersectsFunDups $ map (\(n, (_, s)) -> newDupsStmt (fromJust $ M.lookup n ds) fs s) . M.toList $ fs

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a | b == a    = b
             | otherwise = fixpoint f b
    where b = f a

{-|
Deduplicates function arguments. Some transformations lead to passing
the same value to a function multiple times using multiple arguments.
This optimization detects such situations and selects one of the duplicates
as the canonical one, leaving the others to be cleaned by other
optimizations.

Example:

> fun f (x, y):
>     yield x
>     yield y
>     ret call f (x, y)
> ret call f (0, 0)

Is translated to:

> fun f (x, y):
>     yield x
>     yield x
>     ret call f (x, x)
> ret call f (0, 0)
-}
deduplicateArgs :: IsLowered l => NProg l -> NProg l
deduplicateArgs prog = prog { nProgFuns = M.mapWithKey (\k -> id *** subst (dupSubst $ lookupDup k ds)) fs }
    where
    fs = nProgFuns prog
    ds = fixpoint ((`intersectFunDups` initialds) . newDupsFunMap fs) (initialds `intersectFunDups` initds)
    initialds = initialFunDups fs
    initn = nProgInit prog
    initds = M.singleton initn (newDupsCall M.empty (fst . fromJust $ M.lookup initn fs) (nProgInitParam prog))

