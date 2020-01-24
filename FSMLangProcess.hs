{-# LANGUAGE TemplateHaskell #-}
module FSMLangProcess where

import FSMLang
import THFreeVars
import Prelude
import Control.Monad
import Control.Monad.State
import Control.Lens
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

cutAtEmitsStmts :: (THS.Quasi m, MonadState FunMap (t m), MonadTrans t) => TH.Name -> [Stmt] -> t m [Stmt]
cutAtEmitsStmts n [] = return []
cutAtEmitsStmts n (SEmit e : ss) = do
    n' <- refreshName n
    return $ [SEmit e, SRet (VCall n' undefined)]

cutAtEmitsStmt :: (THS.Quasi m, MonadState FunMap (t m), MonadTrans t) => TH.Name -> Stmt -> t m Stmt
cutAtEmitsStmt n (SBlock stmts) = SBlock <$> cutAtEmitsStmts n stmts
cutAtEmitsStmt n s = SBlock <$> cutAtEmitsStmts n [s]

cutAtEmits :: THS.Quasi m => NProg -> m NProg
cutAtEmits (NProg is fs f1 e1) = do
    fs' <- flip execStateT M.empty $ forM_ (M.toList fs) $ \(n, (p, s)) -> do
        s' <- cutAtEmitsStmt n s
        modify $ M.insert n (p, s)
    return $ NProg is fs' f1 e1


