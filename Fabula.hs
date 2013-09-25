module Fabula (
    Event(..),
    Plot,
    Fabula,
    empty
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Ddta.List as List
import Utils

type Key = Int


data EntityInfo = 
    EntityInfo {
        addr  :: [Key],   --[who i am, who contains me, .....]
        next  :: [Entity],
        prev  :: [Entity]
    }
    deriving(Eq,Ord,Show)

data Fabula = 
    SingleEvent {
        evtPred :: Predicate
    } 
    | Fabula {
        evtPred    :: Predicate,
        table      :: Map.Map Int Entity
        keyCounter :: Key
    }
    deriving (Ord, Show)

type Entity = (EntityInfo, Fabula)

evtInfo :: Fabula -> Predicate
evtInfo (SingleEvent p) = p
evtInfo (Fabula p _ _) = p

instance Eq Fabula where -- weak equality
    a == b = evtInfo a==evtInfo b


--data Plot = EmptyPlot | PlotPiece Predicate Plot
type Plot = [Fabula]


insert :: Fabula -> Fabula -> (Entity,Fabula)
remove :: [Key] -> Fabula -> Fabula
update :: Entity -> Fabula -> Fabula
conseqLink :: (Entity, Entity) -> Fabula -> Fabula
causeLink  :: (Entity, Entity) -> Fabula -> Fabula
conseqOf :: Entity -> [Entity]
causeOf  :: Entity -> [Entity]
abstract   :: Entity -> Fabula -> (Entity,Fabula)
deabstract :: Entity -> Fabula -> Fabula






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


