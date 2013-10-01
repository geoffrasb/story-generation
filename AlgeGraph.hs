module AlgeGraph (
    AlgeGraph(..),
    innerGraph,
    innerEdgeMap
) where

import Utils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Graph.Inductive.Graph as IGraph
import qualified Data.Graph as Graph



-- algebraic graph, a for node, b for edge
data AlgeGraph a b = AlgeGraph { 
    graph     :: Graph.Graph,
    innerVtx2node :: Graph.Vertex -> (a, IGraph.Node, [IGraph.Node]),
    nodeKey2innerVtx :: IGraph.Node -> Maybe Graph.Vertex,
    edgeMap   :: Map.Map (IGraph.Node, IGraph.Node) b
}

innerGraph :: AlgeGraph a b -> Graph.Graph
innerGraph (AlgeGraph g _ _ _) = g
innerEdgeMap :: AlgeGraph a b -> Map.Map (IGraph.Node, IGraph.Node) b
innerEdgeMap (AlgeGraph _ _ _ em) = em


instance IGraph.Graph AlgeGraph where
    empty = AlgeGraph emptyG iv2node node2iv Map.empty
        where (emptyG, iv2node, node2iv) = Graph.graphFromEdges []
    isEmpty (AlgeGraph g _ _ _) = if (Graph.vertices g)==[] then True else False

    match nodeKey (AlgeGraph originG iv2node nodeKey2iv emap) =
        (context, AlgeGraph newG newIv2n newNk2iv newEmap)
        where
            context  = case nodeData of
                        Nothing -> Nothing
                        Just x  -> Just (ladj, nodeKey, fst3 x, radj)
            nodeData = case nodeKey2iv nodeKey of
                            Nothing -> Nothing
                            Just v  -> Just $ iv2node v
            (contextEmap, newEmap) = Map.partitionWithKey 
                                        (\(n1,n2) _ -> n1==nodeKey || n2==nodeKey) emap
            (ladj,radj) = Map.foldWithKey (\(n1,n2) b (la,ra) -> 
                                               if n1==nodeKey then (la, (b,n2):ra)
                                               else ((b,n1):la, ra))
                                          ([],[])
                                          contextEmap
            (newG, newIv2n, newNk2iv) = Graph.graphFromEdges newNodeDatas
            newNodeDatas = foldl (\acc (a,n,ns) -> 
                                     if n==nodeKey then acc else
                                        (a, n, List.delete nodeKey ns) : acc)
                                 []
                                 (map iv2node $ Graph.vertices originG)

    mkGraph lns les =
        AlgeGraph graph iv2node node2iv emap
        where
            (graph,iv2node,node2iv) = Graph.graphFromEdges nodelst
            nodelst = map (\(n,a) -> (a, n, n:(edgeFrom n))) lns
            edgeFrom n = case Map.lookup n edgeTable of
                            Nothing  -> []
                            Just x   -> x
            (edgeTable, emap) = (\f1 f2 (a,b) -> (f1 a, f2 b))
                                    (foldl (\m (n1,n2) -> addRelation m n1 n2) Map.empty)
                                    (Map.fromList)
                                    (unzip $ map (\(n1,n2,b) -> ((n1,n2), ((n1,n2),b))) les)
            addRelation m na nb = Map.alter (alterFunc na) nb $ Map.alter (alterFunc nb) na m
            alterFunc n2 oldData = case oldData of
                                    Nothing -> Just [n2]
                                    Just xs -> Just (n2:xs)

    labNodes AlgeGraph{graph=g, innerVtx2node=iv2n} = 
        map (\inv -> let np=iv2n inv in (snd3 np, fst3 np))
            $ Graph.vertices g


instance IGraph.DynGraph AlgeGraph where
    (ladj, nvtx, a, radj) & (AlgeGraph g iv2n nk2iv emap) = 
        (AlgeGraph new_G new_iv2n new_nk2iv new_emap)
        where
            (new_G, new_iv2n, new_nk2iv) = Graph.graphFromEdges new_nodeDatas
            new_emap = Map.union margin_emap emap
            margin_emap = Map.union 
                            (Map.fromList $ map (\(b,n) -> ((n,nvtx),b)) ladj)
                            (Map.fromList $ map (\(b,n) -> ((nvtx,n),b)) radj)
            new_nodeDatas = case nk2iv nvtx of
                                Nothing -> (a, nvtx, nvtx:linkedNs) : new_nodeDatas'
                                Just _  -> new_nodeDatas'
            new_nodeDatas' = foldl (\acc (a,n,ns) -> 
                                    if n==nvtx then
                                        (a, n, List.union ns $ linkedNs) : acc
                                    else
                                        (a, n, if List.elem n linkedNs then nvtx:ns else ns) : acc
                                    )
                                  [] 
                                  (map iv2n $ Graph.vertices g)
            linkedNs = List.union (map (\(_,n) -> n) ladj) (map (\(_,n) -> n) radj)






