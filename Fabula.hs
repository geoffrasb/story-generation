module Fabula (
    EntityInfo(..),
    Fabula(..),
    Entity,
    Plot
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Graph.Inductive.Graph as IGraph
import qualified AlgeGraph as AG
import Utils

-- if the data type of key changed, only these three lines should be modified.
data Key = Key Int deriving(Show,Eq,Ord)
nextKey (Key a) = Key (a+1)
bottomKey = Key 0

-- KeyGen should be singleton: keyGen
data KeyGen = KeyGen Key deriving(Show)
getKey (KeyGen a) = (a, KeyGen (nextKey a))
keyGen = KeyGen bottomKey


type Event = Predicate
type Plot = [Event]

data AbsEvent = AbsEvent {
    evt :: Event,
    fab :: Fabula,
    imp :: [(IGraph.Node, IGraph.Node)]
}


data Fabula = Fabula (AlgeGraph (Either Event (Fabula, [(IGraph.Node, IGraph.Node)])) ())


{-
putIn
takeOut
update
link
abstract   :: Entity -> Fabula -> (Entity,Fabula)
deabstract :: Entity -> Fabula -> Fabula
-}





-- graph involved algorithms
{-
import qualified Data.Array as Array
import qualified Data.Graph as Graph

data FabulaGraph = FabulaGraph Graph.Graph (Array Int Event)

dfs

graph :: Fabula -> FabulaGraph
graph FabulaElement e =
    FabulaGraph (Array.array (1,0) []) (Array.array (1,1) [(1,e)]) 
graph FabulaNet pri e post = 
    FabulaGraph (Array.array (1,n) edges) (Array.array (1,n) (zip [1..n] elems)
    where 
        f e childf (es,eds) = 
        (elems, edges) = 

inductivism :: FabulaGraph -> Fabula
-}


