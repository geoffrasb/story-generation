import Utils
import qualified AlgeGraph as AG
import qualified Data.Graph.Inductive.Graph as IG

g = IG.empty :: AG.AlgeGraph Int () 

g1 = foldl (\g n -> IG.insNode (n,n) g) g [1,2,3,4,5,6,7]
g2 = foldl (\g (n1,n2) -> IG.insEdge (n1,n2,()) g) g1 [(1,2),(1,3),(7,2),(2,5),(2,6),(3,4)]
g2' = IG.insEdge (3,1,()) g2

mat3 = IG.match 3 g2'

f :: (IG.MContext Int ()) -> IG.Context Int ()
f x = case x of
        Nothing -> ([],1,1,[])
        Just y  -> y
