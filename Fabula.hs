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

data Event = 
    Event {             -- the representation should be considered
        name :: String,
        characters :: [String]
    } deriving (Eq, Ord, Show)

fromPred :: Predicate -> Event
fromPred (n,_,c) = Event{name=n, characters=c}

predOf :: Event -> Predicate
predOf Event{name=n, characters=c} = (n, length c,c)

data Plot = EmptyPlot | PlotPiece Event Plot

type FabulaEntity = (Event,Int,Int) -- eventId,fabulaId
data Entity = 
      Entity {
        eid :: Int,     --event id, every event as a unique id
        fid :: Int,     --fabula id
        e   :: Event,
        next:: [Entity],
        prev:: [Entity]
      }
    | AbstractEntity Fabula
    deriving (Show)

instance Eq Entity where -- weak equality
    Entity{eid=ea, fid=fa} == Entity{eid=eb, fid=fb} = ea==eb && fa==fb
    AbstractEntity fa == AbstractEntity fb           = fa==fb
--need pattern equality


------ Fabula

data Fabula = Fabula {
    key :: Int,
    table :: Map.Map Int Entity
}deriving (Show)

instance Eq Fabula where -- weak equality
    Fabula{key=ka} == Fabula{key=kb} = ka==kb

empty fabId= Fabula fabId Map.empty






------ Operations

--select :: FabulaEntity -> Fabula -> Maybe FabulaEntity
--selectEvent :: Event -> Fabula -> [FabulaEntity]
insert :: Event -> Fabula -> (FabulaEntity,Fabula)
remove :: FabulaEntity -> Fabula -> Fabula
addConseqOf :: FabulaEntity -> Fabula -> Either [FabulaEntity] [Event] -> Fabula
addCauseOf :: FabulaEntity -> Fabula -> Either [FabulaEntity] [Event] -> Fabula
conseqOf :: FabulaEntity -> Fabula -> [FabulaEntity]
conseqOf fe fab = 
    case snd $ Map.lookup fe fab of
        Just a  -> a
        Nothing -> undefined
causeOf :: FabulaEntity -> Fabula -> [FabulaEntity]
causeOf fe fab =
    case fst $ Map.lookup fe fab of
        Just a  -> a
        Nothing -> undefined
abstract
implement






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


