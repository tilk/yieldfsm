{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.LambdaLift(lambdaLift) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Data.Key(mapWithKeyM, forWithKeyM)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

data LLData = LLData {
    _llDataFreeVars :: S.Set TH.Name,
    _llDataEnv :: LLEnv
}

type LLEnv = M.Map TH.Name (TH.Name, [TH.Name])

$(makeLenses ''LLData)

lambdaLiftStmt :: (MonadRefresh m, MonadState FunMap m, MonadReader LLData m) => Stmt -> m Stmt
lambdaLiftStmt   (SLet t ln vs s) = do
    ln' <- refreshName ln
    SLet t ln' <$> lambdaLiftVStmt vs <*> lambdaLiftStmt (renameSingle ln ln' s)
lambdaLiftStmt   (SAssign n vs) = SAssign n <$> lambdaLiftVStmt vs
lambdaLiftStmt s@(SYield _) = return s
lambdaLiftStmt   (SRet vs) = SRet <$> lambdaLiftVStmt vs
lambdaLiftStmt   (SBlock ss) = SBlock <$> mapM lambdaLiftStmt ss
lambdaLiftStmt   (SIf e s1 s2) = SIf e <$> lambdaLiftStmt s1 <*> lambdaLiftStmt s2
lambdaLiftStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> lambdaLiftStmt s) cs
lambdaLiftStmt s@(SNop) = return s
lambdaLiftStmt   (SFun fm s) = do
    fvs <- view llDataFreeVars
    e <- forWithKeyM fm $ \n (p, s') -> (, S.elems $ freeVars s' `S.difference` fvs `underPat` freeVarsPat p) <$> refreshName n
    locally llDataEnv (M.union e) $ do
        fm' <- mapWithKeyM processFun fm
        modify $ M.union $ M.mapKeys (fst . fromJust . flip M.lookup e) fm'
        lambdaLiftStmt s
    where
    processFun n (p, s') = do
        (p', su) <- refreshPat p
        (,) <$> (tupP . (++ [p']) <$> views llDataEnv (map TH.VarP . snd . fromJust . M.lookup n)) <*> lambdaLiftStmt (rename su s')

lambdaLiftVStmt :: (Monad m, MonadReader LLData m) => VStmt -> m VStmt
lambdaLiftVStmt vs@(VExp _) = return vs
lambdaLiftVStmt    (VCall n e) = do
    (n', vs) <- views llDataEnv (fromJust . M.lookup n)
    return $ VCall n' $ tupE $ map TH.VarE vs ++ [e]

lambdaLift :: MonadRefresh m => Prog -> m NProg
lambdaLift prog = do
    (s, fm) <- flip runStateT M.empty $ flip runReaderT (LLData (freeVars $ progBody prog) M.empty) $ lambdaLiftStmt (progBody prog)
    f <- refreshName $ TH.mkName "init"
    return $ NProg (progName prog) (progType prog) (progParams prog) (progInputs prog) (progMemories prog) (M.insert f (tupP [], s) fm) f (tupE []) M.empty

