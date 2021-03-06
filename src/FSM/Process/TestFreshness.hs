{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Freshness check.
-}
module FSM.Process.TestFreshness(testFreshness) where

import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Lang
import FSM.FreeVars
import Prelude

type FrM = State (S.Set TH.Name)

extendSingle :: TH.Name -> FrM ()
extendSingle = extend . S.singleton

extend :: S.Set TH.Name -> FrM ()
extend vs = do
    vs' <- get
    let ivs = S.intersection vs vs'
    if S.null ivs
        then put $ S.union vs vs'
        else error $ "Freshness check failed: " ++ show ivs

testFreshnessStmt :: IsDesugared l => Stmt l -> FrM (Stmt l)
testFreshnessStmt   (SLet t n vs s) = do
    extendSingle n 
    SLet t n vs <$> testFreshnessStmt s
testFreshnessStmt s@(SAssign _ _) = return s
testFreshnessStmt s@(SYield _) = return s
testFreshnessStmt   (SYieldT e s) = SYieldT e <$> testFreshnessStmt s
testFreshnessStmt s@(SRet _) = return s
testFreshnessStmt   (SFun fs s) = SFun <$> mapM testFreshnessCase fs <*> testFreshnessStmt s
testFreshnessStmt   (SBlock ss) = SBlock <$> mapM testFreshnessStmt ss
testFreshnessStmt   (SIf e st sf) = SIf e <$> testFreshnessStmt st <*> testFreshnessStmt sf
testFreshnessStmt   (SCase e cs) = SCase e <$> mapM testFreshnessCase cs
testFreshnessStmt s@(SNop) = return s

testFreshnessPat :: TH.Pat -> FrM TH.Pat
testFreshnessPat p = do
    extend $ boundVars p
    return p
    
testFreshnessCase :: IsDesugared l => (TH.Pat, Stmt l) -> FrM (TH.Pat, Stmt l)
testFreshnessCase (p, s) = (,) <$> testFreshnessPat p <*> testFreshnessStmt s

testFreshnessFunMap :: IsDesugared l => FunMap l -> FunMap l
testFreshnessFunMap = M.map testFreshnessFun
    where
    testFreshnessFun= flip evalState S.empty . testFreshnessCase

{-|
Sanity check for testing if all variable names occuring in the programs are
distinct. This is used because some of the transforms assume this property.
-}
testFreshness :: IsDesugared l => NProg l -> NProg l
testFreshness prog = prog { nProgFuns = testFreshnessFunMap $ nProgFuns prog }

