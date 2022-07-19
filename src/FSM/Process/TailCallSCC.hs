{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Strongly connected components analysis.
-}
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

{-|
Represents strongly connected components and the condensation graph
(the graph whose nodes are SCCs). Each component receives a unique number.
-}
data Partition a = Partition {
    partitionMap :: M.Map a Int,          -- ^ Maps each element to the number of the SCC which contains it.
    partitionSets :: M.Map Int (S.Set a), -- ^ Maps SCC numbers to sets of contained elements.
    partitionEdges :: S.Set (Int, Int)    -- ^ Represents edges in the condensation graph.
} deriving Show

{-|
Gets the number of the SCC containing an element.
-}
partitionLookup :: Ord a => a -> Partition a -> Int
partitionLookup k = fromJust . M.lookup k . partitionMap

{-|
Gets the elements in the SCC with a given number.
-}
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

{-|
Finds strongly connected components in the tail call graph of 'FunMap'.
-}
tailCallSCCFunMap :: IsDesugared l => FunMap l -> Partition TH.Name
tailCallSCCFunMap = tailCallSCCGen callGraphFlat M.keys

tailCallSCCStmt :: IsDesugared l => Stmt l -> Partition TH.Name
tailCallSCCStmt = tailCallSCCGen callGraph (S.toList . funsStmt)

{-|
Finds strongly connected components in the tail call graph of 'Prog'.
-}
tailCallSCC :: IsDesugared l => Prog l -> Partition TH.Name
tailCallSCC = tailCallSCCStmt . progBody

{-|
Finds strongly connected components in the tail call graph of 'NProg'.
-}
tailCallSCCN :: IsDesugared l => NProg l -> Partition TH.Name
tailCallSCCN = tailCallSCCGen callGraphNProg (M.keys . nProgFuns)

