module FSM.Process.TailCallSCC(Partition(..), partitionLookup, tailCallSCCFunMap) where

import FSM.Lang
import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Process.CallGraph
import Data.Graph(stronglyConnComp, flattenSCC)

data Partition a = Partition {
    partitionMap :: M.Map a Int,
    partitionSets :: M.Map Int (S.Set a)
} deriving Show

partitionLookup :: Ord a => a -> Partition a -> Int
partitionLookup k = fromJust . M.lookup k . partitionMap

tailCallSCCFunMap :: FunMap -> Partition TH.Name
tailCallSCCFunMap fs = Partition pMap pSets
    where
    pMap = M.unions $ (\(i, is) -> map (`M.singleton` i) is) =<< sccs
    pSets = M.fromList $ map (id *** S.fromList) sccs
    sccs = zip [0..] $ map flattenSCC $ stronglyConnComp graph
    graph = map toSCC $ M.toList $ M.unionsWith S.union $ (map funToGr (M.keys fs) ++) $ map edgeToGr $ callGraphFlat fs
    edgeToGr e | cgEdgeTail e = M.singleton (cgEdgeSrc e) (S.singleton $ cgEdgeDst e)
               | otherwise = M.empty
    funToGr n = M.singleton n S.empty
    toSCC (n, ns) = (n, n, S.toList ns)


