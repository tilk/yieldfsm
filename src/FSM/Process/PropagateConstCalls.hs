module FSM.Process.PropagateConstCalls(propagateConstCalls) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

data ConstInfo = CISingle (Maybe TH.Exp) | CITuple [Maybe TH.Exp]

mergeConst :: Maybe TH.Exp -> Maybe TH.Exp -> Maybe TH.Exp
mergeConst me1 me2 = do { e1 <- me1; e2 <- me2; mc e1 e2 }
    where
    mc e1 e2 | e1 == e2 = Just e1
             | otherwise = Nothing

mergeConsts :: ConstInfo -> ConstInfo -> ConstInfo
mergeConsts (CISingle e1) (CISingle e2) = CISingle $ mergeConst e1 e2
mergeConsts (CISingle (Just (TH.TupE mes))) (CITuple es) | all isJust mes, length mes == length es = CITuple $ zipWith mergeConst mes es
mergeConsts (CITuple es) (CISingle (Just (TH.TupE mes))) | all isJust mes, length mes == length es = CITuple $ zipWith mergeConst mes es
mergeConsts (CISingle _) _ = CISingle Nothing
mergeConsts _ (CISingle _) = CISingle Nothing
mergeConsts (CITuple es1) (CITuple es2) | length es1 == length es2 = CITuple $ zipWith mergeConst es1 es2
                                        | otherwise = error "Should not happen"

mergeConstsMap :: M.Map TH.Name ConstInfo -> M.Map TH.Name ConstInfo -> M.Map TH.Name ConstInfo
mergeConstsMap = M.unionWith mergeConsts

genConst :: TH.Exp -> Maybe TH.Exp
genConst e | S.null $ freeVars e = Just e
           | otherwise = Nothing

genConsts :: TH.Exp -> ConstInfo
genConsts (TH.TupE mes) | all isJust mes = CITuple $ map (genConst . fromJust) mes
                        | otherwise = error "Should not happen"
genConsts e = CISingle $ genConst e

mkSubstConst :: TH.Pat -> Maybe TH.Exp -> M.Map TH.Name TH.Exp
mkSubstConst _ Nothing = M.empty
mkSubstConst (TH.VarP n) (Just e) = M.singleton n e
mkSubstConst _ _ = M.empty

mkSubstConsts :: TH.Pat -> Maybe ConstInfo -> M.Map TH.Name TH.Exp
mkSubstConsts _ Nothing = M.empty
mkSubstConsts (TH.TupP ps) (Just (CITuple mes)) = M.unions $ zipWith mkSubstConst ps mes
mkSubstConsts p (Just (CISingle me)) = mkSubstConst p me
mkSubstConsts p (Just (CITuple mes)) = mkSubstConst p (tupE <$> sequence mes)

processFunMap :: M.Map TH.Name ConstInfo -> FunMap -> FunMap
processFunMap cs = M.mapWithKey $ \n (p, s) -> (p, subst (mkSubstConsts p (M.lookup n cs)) s)

makeConstsMapCall :: TH.Name -> TH.Exp -> M.Map TH.Name ConstInfo
makeConstsMapCall n e = M.singleton n (genConsts e)

makeConstsMapVStmt :: VStmt -> M.Map TH.Name ConstInfo
makeConstsMapVStmt (VExp _) = M.empty
makeConstsMapVStmt (VCall n e) = M.singleton n (genConsts e)

makeConstsMapStmt :: Stmt -> M.Map TH.Name ConstInfo
makeConstsMapStmt (SLet _ _ vs s) = makeConstsMapVStmt vs `mergeConstsMap` makeConstsMapStmt s
makeConstsMapStmt (SAssign _ vs) = makeConstsMapVStmt vs
makeConstsMapStmt (SYield _) = M.empty
makeConstsMapStmt (SRet vs) = makeConstsMapVStmt vs
makeConstsMapStmt (SFun fs s) = makeConstsMapStmt s `mergeConstsMap` makeConstsMapFunMap fs
makeConstsMapStmt (SBlock ss) = M.unionsWith mergeConsts $ map makeConstsMapStmt ss
makeConstsMapStmt (SIf _ st sf) = makeConstsMapStmt st `mergeConstsMap` makeConstsMapStmt sf
makeConstsMapStmt (SCase _ cs) = M.unionsWith mergeConsts $ map (makeConstsMapStmt . snd) cs
makeConstsMapStmt  SNop = M.empty

makeConstsMapFunMap :: FunMap -> M.Map TH.Name ConstInfo
makeConstsMapFunMap = M.unionsWith mergeConsts . map (makeConstsMapStmt . snd) . M.elems

propagateConstCalls :: NProg -> NProg
propagateConstCalls prog = prog {
        nProgFuns = processFunMap cs $ nProgFuns prog
    }
    where
    cs = makeConstsMapFunMap (nProgFuns prog) `mergeConstsMap` makeConstsMapCall (nProgInit prog) (nProgInitParam prog)

