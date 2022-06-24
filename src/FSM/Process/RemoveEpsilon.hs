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

data REData l = REData {
    _reDataFunMap :: FunMap l,
    _reDataInputVars :: S.Set TH.Name,
    _reDataEmitted :: Bool,
    _reDataVisited :: S.Set TH.Name
}

$(makeLenses ''REData)

removeEpsilonStmt :: (IsLowered l, MonadRefresh f, MonadReader (REData l) f, MonadState (M.Map TH.Name (TH.Pat, Stmt l)) f) =>
                     Stmt l -> f (Stmt l)
removeEpsilonStmt   (SIf e st sf) = SIf e <$> removeEpsilonStmt st <*> removeEpsilonStmt sf
removeEpsilonStmt   (SLet t ln vs s) = SLet t ln vs <$> removeEpsilonStmt s
removeEpsilonStmt   (SCase e cs) = SCase e <$> mapM cf cs where
    cf (p, s) = (p,) <$> removeEpsilonStmt s
removeEpsilonStmt   (SYieldT e s) = SYieldT e <$> locally reDataEmitted (const True) (locally reDataVisited (const S.empty) $ removeEpsilonStmt s)
removeEpsilonStmt s@(SRet (VCall f e)) = do
    (p, s') <- views reDataFunMap $ fromJust . M.lookup f
    em <- view reDataEmitted
    ivs <- view reDataInputVars
    b <- gets (M.member f)
    if em && (b || emittingStmt s' || not (S.null $ S.intersection ivs $ freeVars s')) then removeEpsilonFrom f >> return s
    else do
        vis <- view reDataVisited
        when (S.member f vis) $ error $ "cycle without yield: " ++ show vis
        (p', su) <- refreshPat p
        locally reDataVisited (S.insert f) $ SCase e <$> (return . (p',) <$> removeEpsilonStmt (rename su s'))
removeEpsilonStmt s = error $ "removeEpsilonStmt statement not in tree form: " ++ show s

removeEpsilonFrom :: (IsLowered l, MonadRefresh f, MonadReader (REData l) f, MonadState (M.Map TH.Name (TH.Pat, Stmt l)) f) =>
                     TH.Name -> f ()
removeEpsilonFrom f = do
    (p, s) <- views reDataFunMap $ fromJust . M.lookup f
    b <- gets (M.member f)
    unless b $ do
        modify $ M.insert f (p, error "BUG in removeEpsilon")
        s' <- locally reDataEmitted (const False) $ locally reDataVisited (const S.empty) $ removeEpsilonStmt s
        modify $ M.insert f (p, s')

removeEpsilon :: (IsLowered l, MonadRefresh m) => NProg l -> m (NProg l)
removeEpsilon prog = do
    fs' <- flip execStateT M.empty $ flip runReaderT (REData (nProgFuns prog) (boundVars $ nProgInputs prog) False S.empty) $ removeEpsilonFrom (nProgInit prog)
    return $ prog { nProgFuns = fs' }

