module Utils 
    ( fst3
    , snd3
    , thd3
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List




fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a


{-

    dfs_tagging :: (Eq a) => a -> (a -> [a]) -> ([a], Int) -> [(a,Int,Int)]
    dfs_tagging t childf (visited,i) =
        let children = childf t
            filtered_children = children List.\\ visited
            (children_tagging,_) = 
                foldl (\(taglst,ni) ct ->
                          let ct_tagging = dfs_tagging ct (childf) (t:visited,ni) in  
                              (ct_tagging++taglst, 1+(thd3 $ head ct_tagging)))
                      ([],i+1)
                      filtered_children
        in
            if filtered_children == [] then
                [(t,i,i+1)]
            else
                (t, i,1+(thd3 $ head children_tagging)) : children_tagging

    topoSort :: (Eq a) => a -> (a -> [a]) -> [a]
    topoSort tree childf = 
        let
            tags = dfs_tagging tree (childf) ([],1)
            tmap = Map.fromList $ map (\(t,_,endTag) -> (endTag,t)) tags
            sorted_tags = List.sort $ map thd3 tags
        in
            foldl (\accResult i -> case Map.lookup i tmap of
                                        Just t -> t:accResult
                                        Nothing -> undefined)
                  []
                  sorted_tags

    collect_elem_edge_from :: (Eq a) => a -> (a -> [a]) -> [a] -> ([a], [(a, a)])
    collect_elem_edge_from e childf es = 
        let children = childf e
            filtered_children = children List.\\ es 
            new_eds  = [(e,pos) | pos <- children]
            new_es = e:es
        in
            if children == [] then
                (new_es,[])
            else 
                if filtered_children == [] then
                    (new_es, new_eds)
                else
                    foldl (\(esa,edsa) (esb,edsb) -> (List.union esa esb, edsa++edsb))
                          (new_es, new_eds)
                          (map (\nc -> collect_elem_edge_from nc (childf) new_es ) filtered_children)
-}

