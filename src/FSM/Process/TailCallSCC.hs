module FSM.Process.TailCallSCC(Partition(..), partitionSet, partitionLookup, tailCallSCCFunMap, tailCallSCC, tailCallSCCN) where

import FSM.Lang
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Process.CallGraph
import Data.Graph(stronglyConnComp, flattenSCC)

funsStmt :: IsDesugared l => Stmt l -> S.Set TH.Name
funsStmt (SLet _ _ _ s) = funsStmt s
funsStmt (SAssign _ _) = S.empty
funsStmt (SYield _) = S.empty
funsStmt (SYieldT _ s) = funsStmt s
funsStmt (SRet _) = S.empty
funsStmt (SFun fs s) = funsStmt s `S.union` S.unions (map (funsStmt . snd . snd) $ M.toList fs) `S.union` M.keysSet fs
funsStmt (SBlock ss) = S.unions $ map funsStmt ss
funsStmt (SIf _ st sf) = funsStmt st `S.union` funsStmt sf
funsStmt (SCase _ cs) = S.unions $ map (funsStmt . snd) cs
funsStmt SNop = S.empty

data Partition a = Partition {
    partitionMap :: M.Map a Int,
    partitionSets :: M.Map Int (S.Set a),
    partitionEdges :: S.Set (Int, Int)
} deriving Show

partitionLookup :: Ord a => a -> Partition a -> Int
partitionLookup k = fromJust . M.lookup k . partitionMap

partitionSet :: Int -> Partition a -> S.Set a
partitionSet n = fromJust . M.lookup n . partitionSets

tailCallSCCGen :: (a -> CG) -> (a -> [TH.Name]) -> a -> Partition TH.Name
tailCallSCCGen gr fs x = Partition pMap pSets pEdges
    where
    pMap = M.unions $ (\(i, is) -> map (`M.singleton` i) is) =<< sccs
    pSets = M.fromList $ map (id *** S.fromList) sccs
    pEdges = S.fromList $ filter (uncurry (/=)) $ map (pLookup . cgEdgeSrc &&& pLookup . cgEdgeDst) edges
    pLookup = fromJust . flip M.lookup pMap
    sccs = zip [0..] $ map flattenSCC $ stronglyConnComp graph
    edges = filter cgEdgeTail $ gr x
    graph = map toSCC $ M.toList $ M.unionsWith S.union $ (map funToGr (fs x) ++) $ map edgeToGr edges
    edgeToGr e = M.singleton (cgEdgeSrc e) (S.singleton $ cgEdgeDst e)
    funToGr n = M.singleton n S.empty
    toSCC (n, ns) = (n, n, S.toList ns)

tailCallSCCFunMap :: IsDesugared l => FunMap l -> Partition TH.Name
tailCallSCCFunMap = tailCallSCCGen callGraphFlat M.keys

tailCallSCCStmt :: IsDesugared l => Stmt l -> Partition TH.Name
tailCallSCCStmt = tailCallSCCGen callGraph (S.toList . funsStmt)

tailCallSCC :: IsDesugared l => Prog l -> Partition TH.Name
tailCallSCC = tailCallSCCStmt . progBody

tailCallSCCN :: IsDesugared l => NProg l -> Partition TH.Name
tailCallSCCN = tailCallSCCGen callGraphNProg (M.keys . nProgFuns)

