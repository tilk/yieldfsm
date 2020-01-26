{-# LANGUAGE TemplateHaskell #-}
module FSMLangProcess where

import FSMLang
import FSMFreeVars
import Prelude
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

{-
data LifterState = LifterState {
    lifterFunEnv :: M.Map TH.Name TH.Name,
    lifterFuns :: M.Map TH.Name (TH.Pat, Stmt)
}

$(makeLenses ''LifterState)

lambdaLift (SSeq s1 s2) = SSeq <$> lambdaLift s1 <*> lambdaLift s2
lambdaLift s = return s
-}

refreshName :: (THS.Quasi m, MonadTrans t) => TH.Name -> t m TH.Name
refreshName n = lift $ THS.qNewName $ TH.nameBase n

simpleStmt SNop = True
simpleStmt (SRet (VCall _ _)) = True
simpleStmt _ = False

cutBlocksStmt :: (THS.Quasi m, MonadState FunMap (t m), MonadTrans t) => TH.Name -> Stmt -> Stmt -> t m Stmt
cutBlocksStmt n SNop s' = return s'
cutBlocksStmt n (SRet vs) s' = return $ SRet vs
cutBlocksStmt n (SBlock []) s' = return s'
cutBlocksStmt n (SBlock [s]) s' = cutBlocksStmt n s s'
cutBlocksStmt n (SBlock (s:ss)) s' = do
    s'' <- cutBlocksStmt n (SBlock ss) s'
    cutBlocksStmt n s s''
cutBlocksStmt n (SEmit e) s' | simpleStmt s' =
    return $ SBlock [SEmit e, s']
cutBlocksStmt n (SIf e st sf) s' | simpleStmt s' =
    SIf e <$> cutBlocksStmt n st s' <*> cutBlocksStmt n sf s'
cutBlocksStmt n (SLet ln vs s) s' | simpleStmt s' = do
    ln' <- refreshName ln
    SLet ln' vs <$> cutBlocksStmt n (renameStmt (M.singleton ln ln') s) s'
cutBlocksStmt n s s' = do
    let vs = S.toList $ freeVarsStmt s'
    n' <- refreshName n
    modify $ M.insert n' (TH.TupP $ map TH.VarP vs, s')
    cutBlocksStmt n s (SRet (VCall n' (TH.TupE $ map TH.VarE vs)))

{-
cutBlocksStmts :: (THS.Quasi m, MonadState FunMap (t m), MonadTrans t) => TH.Name -> [Stmt] -> t m Stmt
cutBlocksStmts n [] = return SNop
cutBlocksStmts n (s:ss) = cutBlocksStmt n s ss

cutBlocksStmt :: (THS.Quasi m, MonadState FunMap (t m), MonadTrans t) => TH.Name -> Stmt -> [Stmt] -> t m Stmt
cutBlocksStmt n (SLet n' vs s) ss = 
cutBlocksStmt n (SBlock []) ss = cutBlocksStmts n ss
cutBlocksStmt n (SBlock (s:ss')) ss = cutBlocksStmt n s (ss' ++ ss)
cutBlocksStmt n (SEmit e) [SRet (VCall n' e')] = return $ SBlock [SEmit e, SRet (VCall n' e')]
cutBlocksStmt n (SEmit e) ss = do
    let vs = S.toList $ freeVarsStmts ss
    n' <- refreshName n
    ss' <- cutBlocksStmts n ss
    modify $ M.insert n' (TH.TupP $ map TH.VarP vs, ss')
    return $ SBlock [SEmit e, SRet (VCall n' (TH.TupE $ map TH.VarE vs))]
-}
cutBlocks :: THS.Quasi m => NProg -> m NProg
cutBlocks (NProg is fs f1 e1) = do
    fs' <- flip execStateT M.empty $ forM_ (M.toList fs) $ \(n, (p, s)) -> do
        s' <- cutBlocksStmt n s SNop
        modify $ M.insert n (p, s')
    return $ NProg is fs' f1 e1


