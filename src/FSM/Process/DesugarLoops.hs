{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>
-}
{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.DesugarLoops(desugarLoops) where

import Prelude
import FSM.Lang
import FSM.LangQ
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh
import FSM.Util.Qlift

data RData = RData {
    _rDataLoop :: Maybe TH.Name
}

$(makeLenses ''RData)

ltName :: LoopType -> String
ltName LoopForever = "forever"
ltName (LoopWhile _ _ _) = "while"
ltName (LoopRepeat _ _) = "repeat"

wtIfQ :: WhileType -> TH.Exp -> StmtQ t -> StmtQ t -> StmtQ t
wtIfQ WhileWhile = sIf . return
wtIfQ WhileUntil = flip . sIf . return

desugarLoopsLoop :: (Qlift m, MonadRefresh m, MonadReader RData m) => TH.Name -> LoopType -> Stmt LvlFull -> m (Stmt LvlFull)
desugarLoopsLoop f LoopForever s = do
    let scall = SRet $ VCall f $ TH.TupE []
    return $ SFun (M.singleton f (TH.TupP [], SBlock [s, scall])) scall
desugarLoopsLoop f (LoopRepeat IterWhile e) s = do
    k <- makeName "k"
    qlift $ sLet VarMut k (vExp $ return e) $
            sFun (M.singleton f (TH.tupP [], sIf [| $(TH.varE k) /= 0 |]
                                                 (sBlock [return s, sAssign k [| $(TH.varE k) - 1 |], sRet $ vCall f $ TH.tupE []])
                                                 (sRet $ vExp $ TH.tupE [])))
                 (sLet VarLet (TH.mkName "_") (vCall f $ TH.tupE []) sNop)
desugarLoopsLoop f (LoopRepeat IterDoWhile e) s = do
    k <- makeName "k"
    qlift $ sLet VarMut k (vExp [| $(return e) - 1 |]) $
            sFun (M.singleton f (TH.tupP [], sBlock $ [return s, sIf [| $(TH.varE k) /= 0 |]
                                                                     (sBlock [sAssign k [| $(TH.varE k) - 1 |], sRet $ vCall f $ TH.tupE []])
                                                                     (sRet $ vExp $ TH.tupE [])]))
                 (sLet VarLet (TH.mkName "_") (vCall f $ TH.tupE []) sNop)
desugarLoopsLoop f (LoopWhile IterWhile wt e) s = do
    qlift $ sFun (M.singleton f (TH.tupP [], wtIfQ wt e (sBlock [return s, sRet $ vCall f $ TH.tupE []]) (sRet $ vExp $ TH.tupE [])))
                 (sLet VarLet (TH.mkName "_") (vCall f $ TH.tupE []) sNop)
desugarLoopsLoop f (LoopWhile IterDoWhile wt e) s = do
    qlift $ sFun (M.singleton f (TH.tupP [], sBlock $ [return s, wtIfQ wt e (sRet $ vCall f $ TH.tupE []) (sRet $ vExp $ TH.tupE [])]))
                 (sLet VarLet (TH.mkName "_") (vCall f $ TH.tupE []) sNop)

desugarLoopsStmt :: (Qlift m, MonadRefresh m, MonadReader RData m) => Stmt LvlLoops -> m (Stmt LvlFull)
desugarLoopsStmt (SLoop lt s) = do
    f <- makeName $ ltName lt
    s' <- locally rDataLoop (const $ Just f) $ desugarLoopsStmt s
    desugarLoopsLoop f lt s'
desugarLoopsStmt (SBreak bt) = do
    l <- view rDataLoop
    case l of
        Nothing -> error "Break/continue outside a loop"
        Just i -> case bt of
            BrkCont -> return $ SRet $ VCall i (TH.TupE [])
            BrkBrk -> return $ SRet $ VExp $ TH.TupE []
desugarLoopsStmt (SFun fs s) = SFun <$> mapM (\(p, s') -> locally rDataLoop (const Nothing) $ (p,) <$> desugarLoopsStmt s') fs <*> desugarLoopsStmt s
desugarLoopsStmt (SLet t n vs s) = SLet t n vs <$> desugarLoopsStmt s
desugarLoopsStmt (SAssign n e) = return $ SAssign n e
desugarLoopsStmt (SYield e) = return $ SYield e
desugarLoopsStmt (SRet vs) = do
    l <- view rDataLoop
    case l of
        Nothing -> return $ SRet vs 
        Just _ -> error "Return in loops currently unsupported"
desugarLoopsStmt (SBlock ss) = SBlock <$> mapM desugarLoopsStmt ss
desugarLoopsStmt (SIf e st sf) = SIf e <$> desugarLoopsStmt st <*> desugarLoopsStmt sf
desugarLoopsStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> desugarLoopsStmt s) cs
desugarLoopsStmt (SNop) = return SNop

desugarLoops :: (Qlift m, MonadRefresh m) => Prog LvlLoops -> m (Prog LvlFull)
desugarLoops prog = do
    b <- flip runReaderT (RData Nothing) $ desugarLoopsStmt $ progBody prog
    return $ prog { progBody = b }

