{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

This module defines the lambda-lifting transformation.
-}
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

lambdaLiftStmt :: (MonadRefresh m, MonadState (FunMap LvlLifted) m, MonadReader LLData m) => Stmt LvlFull -> m (Stmt LvlLifted)
lambdaLiftStmt (SLet t ln vs s) = do
    ln' <- refreshName ln
    SLet t ln' <$> lambdaLiftVStmt vs <*> lambdaLiftStmt (renameSingle ln ln' s)
lambdaLiftStmt (SAssign n e) = return $ SAssign n e
lambdaLiftStmt (SYield e) = return $ SYield e
lambdaLiftStmt (SRet vs) = SRet <$> lambdaLiftVStmt vs
lambdaLiftStmt (SBlock ss) = SBlock <$> mapM lambdaLiftStmt ss
lambdaLiftStmt (SIf e s1 s2) = SIf e <$> lambdaLiftStmt s1 <*> lambdaLiftStmt s2
lambdaLiftStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> lambdaLiftStmt s) cs
lambdaLiftStmt (SNop) = return SNop
lambdaLiftStmt (SFun fm s) = do
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

{-|
Performs the lambda-lifting transformation.
It assumes that the input program has no non-local references to mutable
variables. The result is in lambda-lifted form.

Example:

> let x = 1
> fun f y:
>     yield y
>     ret call f (x + y)
> let z = 0
> ret call f z

Is translated to:

> fun init ():
>     let x = 1
>     let z = 0
>     ret call f (x, z)
> fun f (x, y):
>     yield y
>     ret call f (x, x + y)
> ret call init ()

-}
lambdaLift :: MonadRefresh m => Prog LvlFull -> m (NProg LvlLifted)
lambdaLift prog = do
    (s, fm) <- flip runStateT M.empty $ flip runReaderT (LLData (freeVars $ progBody prog) M.empty) $ lambdaLiftStmt (progBody prog)
    f <- refreshName $ TH.mkName "init"
    return $ NProg (progName prog) (progType prog) (progParams prog) (progInputs prog) (M.insert f (tupP [], s) fm) f (tupE []) M.empty

