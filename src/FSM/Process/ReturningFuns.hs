{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

This module defines returning function analysis.
-}
module FSM.Process.ReturningFuns(returningFuns, returningFunsFlat) where

import FSM.Lang
import Prelude
import Control.Applicative(liftA2)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Process.CallGraph

saturateSet :: Ord k => (k -> S.Set k) -> S.Set k -> S.Set k
saturateSet m = flip g S.empty where
    g s = foldr (.) id (map f (S.toList s))
    f n s | n `S.member` s = s
          | otherwise = g (m n) (S.insert n s)

directRetFunMap :: IsDesugared l => FunMap l -> S.Set TH.Name
directRetFunMap fs = S.fromList [n | (n, (_, s')) <- M.toList fs, isReturningStmt s']
                     `S.union` (S.unions $ map (directRet . snd . snd) $ M.toList fs)

directRet :: IsDesugared l => Stmt l -> S.Set TH.Name
directRet SNop = S.empty
directRet (SYield _) = S.empty
directRet (SYieldT _ s) = directRet s
directRet (SLet _ _ _ s) = directRet s
directRet (SAssign _ _) = S.empty
directRet (SRet _) = S.empty
directRet (SBlock ss) = S.unions $ map directRet ss
directRet (SIf _ st sf) = directRet st `S.union` directRet sf
directRet (SCase _ cs) = S.unions $ map (directRet . snd) cs
directRet (SFun fs s) = directRet s `S.union` directRetFunMap fs

returningFunsH :: CG -> S.Set TH.Name -> S.Set TH.Name
returningFunsH cg ns = saturateSet (flip (M.findWithDefault S.empty) tailCalled) ns
    where
    tailCalled = M.fromListWith S.union $ map (\e -> (cgEdgeDst e, S.singleton $ cgEdgeSrc e)) $ filter cgEdgeTail cg

{-|
Performs the returning function analysis. A function is returning if it
contains a value return statement or tail calls another returning function.
Returns the set of names of returning functions.
-}
returningFuns :: IsDesugared l => Stmt l -> S.Set TH.Name
returningFuns s = returningFunsH (callGraph s) (directRet s)

{-|
Performs the returning function analysis. Variant for 'FunMap',
used on lambda-lifted programs.
-}
returningFunsFlat :: IsDesugared l => FunMap l -> S.Set TH.Name
returningFunsFlat fs = returningFunsH (callGraphFlat fs) (directRetFunMap fs)

isReturningStmt :: IsDesugared l => Stmt l -> Bool
isReturningStmt s = isReturningStmtM s True

isReturningStmtM :: IsDesugared l => Stmt l -> Bool -> Bool
isReturningStmtM SNop = id
isReturningStmtM (SYield _) = id
isReturningStmtM (SYieldT _ s) = isReturningStmtM s
isReturningStmtM (SLet _ _ _ s) = isReturningStmtM s
isReturningStmtM (SAssign _ _) = id
isReturningStmtM (SBlock ss) = foldr (.) id $ isReturningStmtM <$> ss
isReturningStmtM (SIf _ st sf) = (||) <$> isReturningStmtM st <*> isReturningStmtM sf
isReturningStmtM (SCase _ cs) = foldr1 (liftA2 (||)) $ isReturningStmtM <$> map snd cs
isReturningStmtM (SRet (VExp _)) = const True
isReturningStmtM (SRet (VCall _ _)) = const False
isReturningStmtM (SFun _ s) = isReturningStmtM s

