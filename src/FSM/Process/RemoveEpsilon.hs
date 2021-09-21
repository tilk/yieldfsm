{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.RemoveEpsilon(removeEpsilon) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

data REData = REData {
    _reDataFunMap :: FunMap,
    _reDataInputVars :: S.Set TH.Name,
    _reDataEmitted :: Bool
}

$(makeLenses ''REData)

removeEpsilonStmt :: (MonadRefresh f, MonadReader REData f, MonadState (M.Map TH.Name (TH.Pat, Stmt)) f) =>
                     Stmt -> f Stmt
removeEpsilonStmt s@SNop = return s
removeEpsilonStmt s@(SYield _) = return s
removeEpsilonStmt   (SIf e st sf) = SIf e <$> removeEpsilonStmt st <*> removeEpsilonStmt sf
removeEpsilonStmt   (SLet t ln vs s) = SLet t ln vs <$> removeEpsilonStmt s
removeEpsilonStmt   (SCase e cs) = SCase e <$> mapM cf cs where
    cf (p, s) = (p,) <$> removeEpsilonStmt s
removeEpsilonStmt   (SBlock [SYield e, s]) = (\s' -> SBlock [SYield e, s']) <$> locally reDataEmitted (const True) (removeEpsilonStmt s)
removeEpsilonStmt s@(SRet (VCall f e)) = do
    (p, s') <- views reDataFunMap $ fromJust . M.lookup f
    em <- view reDataEmitted
    ivs <- view reDataInputVars
    b <- gets (M.member f)
    if em && (b || emittingStmt s' || not (S.null $ S.intersection ivs $ freeVars s')) then removeEpsilonFrom f >> return s
    else do
        (p', su) <- refreshPat p
        SCase e <$> (return . (p',) <$> removeEpsilonStmt (renameStmt su s'))
removeEpsilonStmt s = error $ "removeEpsilonStmt statement not in tree form: " ++ show s

removeEpsilonFrom :: (MonadRefresh f, MonadReader REData f, MonadState (M.Map TH.Name (TH.Pat, Stmt)) f) =>
                     TH.Name -> f ()
removeEpsilonFrom f = do
    (p, s) <- views reDataFunMap $ fromJust . M.lookup f
    b <- gets (M.member f)
    unless b $ do
        modify $ M.insert f (p, SNop)
        s' <- locally reDataEmitted (const False) $ removeEpsilonStmt s
        modify $ M.insert f (p, s')

removeEpsilon :: MonadRefresh m => NProg -> m NProg
removeEpsilon prog = do
    fs' <- flip execStateT M.empty $ flip runReaderT (REData (nProgFuns prog) (boundVars $ nProgInputs prog) False) $ removeEpsilonFrom (nProgInit prog)
    return $ prog { nProgFuns = fs' }

