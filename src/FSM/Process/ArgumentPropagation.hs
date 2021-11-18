module FSM.Process.ArgumentPropagation(argumentPropagation) where

import FSM.Lang
import FSM.FreeVars
import FSM.Process.CallGraph
import Prelude
import Data.Maybe
import Data.Tuple(swap)
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import Data.Graph(stronglyConnComp, flattenSCC)

data GNode = GNode {
    gNodeFun :: TH.Name,
    gNodeExp :: TH.Exp
} deriving (Ord, Eq, Show)

-- TODO: support ConP, LitP... see SimplifyCase
nodePairs :: CGEdge -> TH.Pat -> [(GNode, GNode)]
nodePairs ce p = f (cgEdgeArg ce) p
    where
    f (TH.TupE mes) (TH.TupP ps) | length mes == length ps, all isJust mes = concat $ zipWith f (map fromJust mes) ps
    f e (TH.VarP n) = [(GNode (cgEdgeSrc ce) e, GNode (cgEdgeDst ce) (TH.VarE n))]
    f _ _ = []

makeGraph :: IsDesugared l => FunMap l -> CG -> [(GNode, GNode)]
makeGraph fs = concatMap edgeVals
    where
    edgeVals e | Just (p, _) <- M.lookup (cgEdgeDst e) fs = nodePairs e p
               | otherwise = []

edgeMap :: [(GNode, GNode)] -> M.Map GNode (S.Set GNode)
edgeMap edges = M.unionsWith S.union . (map (flip M.singleton S.empty . snd) edges ++) . map (M.map S.singleton . uncurry M.singleton) $ edges

findSCC :: [(GNode, GNode)] -> [S.Set GNode]
findSCC = map (S.fromList . flattenSCC) . stronglyConnComp . map toSCC . M.toList . edgeMap
    where
    toSCC (n, ns) = (n, n, S.toList ns)

extendSub :: M.Map GNode (S.Set GNode) -> S.Set GNode -> M.Map GNode TH.Exp -> M.Map GNode TH.Exp
extendSub edges scc sub | [Just e] <- preds = M.fromSet (const e) scc `M.union` sub
                        | otherwise = sub
    where
    preds = S.toList $ S.map nodeMConst $ S.filter (not . (`S.member` scc)) $ S.unions $ map (maybe S.empty id . flip M.lookup edges) $ S.toList scc
    nodeMConst node | isConstantExpr (gNodeExp node) = Just (gNodeExp node)
                    | otherwise = M.lookup node sub

subPerFun :: M.Map GNode TH.Exp -> M.Map TH.Name (M.Map TH.Name TH.Exp)
subPerFun = M.fromListWith M.union . map (gNodeFun . fst &&& (f . (gNodeExp . fst &&& snd))) . M.toList
    where
    f (TH.VarE n, e) = M.singleton n e
    f _ = M.empty

extendFuns :: IsDesugared l => M.Map TH.Name (M.Map TH.Name TH.Exp) -> FunMap l -> FunMap l
extendFuns sub fs = M.mapWithKey f fs
    where
    f n (p, s) = (p, foldr g s (M.toList $ maybe M.empty id $ M.lookup n sub))
    g (n, e) s = SLet VarLet n (VExp e) s

argumentPropagation :: IsDesugared l => NProg l -> NProg l
argumentPropagation prog = prog {
        nProgFuns = extendFuns sub $ nProgFuns prog
    }
    where
    gr = makeGraph (nProgFuns prog) (callGraphNProg prog)
    sub = subPerFun $ foldr (extendSub edges) M.empty $ findSCC gr
    edges = edgeMap $ map swap gr


