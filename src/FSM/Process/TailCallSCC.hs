module FSM.Process.TailCallSCC(Partition(..), partitionLookup, tailCallSCCFunMap, tailCallSCC, tailCallSCCN) where

import FSM.Lang
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Process.CallGraph
import Data.Graph(stronglyConnComp, flattenSCC)

funsStmt :: Stmt -> S.Set TH.Name
funsStmt (SLet _ _ _ s) = funsStmt s
funsStmt (SAssign _ _) = S.empty
funsStmt (SYield _) = S.empty
funsStmt (SRet _) = S.empty
funsStmt (SFun fs s) = funsStmt s `S.union` S.unions (map (funsStmt . snd . snd) $ M.toList fs) `S.union` M.keysSet fs
funsStmt (SBlock ss) = S.unions $ map funsStmt ss
funsStmt (SIf _ st sf) = funsStmt st `S.union` funsStmt sf
funsStmt (SCase _ cs) = S.unions $ map (funsStmt . snd) cs
funsStmt SNop = S.empty

data Partition a = Partition {
    partitionMap :: M.Map a Int,
    partitionSets :: M.Map Int (S.Set a)
} deriving Show

partitionLookup :: Ord a => a -> Partition a -> Int
partitionLookup k = fromJust . M.lookup k . partitionMap

tailCallSCCGen :: (a -> CG) -> (a -> [TH.Name]) -> a -> Partition TH.Name
tailCallSCCGen gr fs x = Partition pMap pSets
    where
    pMap = M.unions $ (\(i, is) -> map (`M.singleton` i) is) =<< sccs
    pSets = M.fromList $ map (id *** S.fromList) sccs
    sccs = zip [0..] $ map flattenSCC $ stronglyConnComp graph
    graph = map toSCC $ M.toList $ M.unionsWith S.union $ (map funToGr (fs x) ++) $ map edgeToGr $ gr x
    edgeToGr e | cgEdgeTail e = M.singleton (cgEdgeSrc e) (S.singleton $ cgEdgeDst e)
               | otherwise = M.empty
    funToGr n = M.singleton n S.empty
    toSCC (n, ns) = (n, n, S.toList ns)

tailCallSCCFunMap :: FunMap -> Partition TH.Name
tailCallSCCFunMap = tailCallSCCGen callGraphFlat M.keys

tailCallSCCStmt :: Stmt -> Partition TH.Name
tailCallSCCStmt = tailCallSCCGen callGraph (S.toList . funsStmt)

tailCallSCC :: Prog -> Partition TH.Name
tailCallSCC = tailCallSCCStmt . progBody

tailCallSCCN :: NProg -> Partition TH.Name
tailCallSCCN = tailCallSCCGen callGraphNProg (M.keys . nProgFuns)

