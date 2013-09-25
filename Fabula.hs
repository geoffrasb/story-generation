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
    } 
    | AbsEvent {
        name :: String,
        characters :: [String],
        innerFab :: Fabula
    }
    deriving (Ord, Show)

evtName :: Event -> String
evtName (Event n _) = n
evtName (AbsEvent n _ _) = n
invoChar :: Event -> [String]
invoChar (Event _ c) = c
invoChar (AbsEvent _ c _) = c

instance Eq Event where
    a == b = (evtName a==evtName b) && (invoChar a==invoChar b)

fromPred :: Predicate -> Event
fromPred (n,_,c) = Event{name=n, characters=c}
predOf :: Event -> Predicate
predOf Event{name=n, characters=c} = (n, length c,c)

data Plot = EmptyPlot | PlotPiece Event Plot



------ Fabula


data EntityInfo = EntityInfo {
    eid      :: Int, 
    fid      :: [Int],
    next     :: [Entity],
    prev     :: [Entity]
}deriving(Eq,Ord,Show)


data Fabula = Fabula {
    key     :: Int,
    evtKeyCounter :: Int,
    table   :: Map.Map Int (EntityInfo, Event)
}deriving (Show)

instance Eq Fabula where -- weak equality
    Fabula{key=ka} == Fabula{key=kb} = ka==kb

emptyFabula fabId= Fabula fabId 0 Map.empty


---------- Container

data Container = Container {
    key     :: Int,
    fabKeyCounter :: Int,
    table   :: Map.Map Int Fabula
}

emptyContainer = Container 0 Map.empty

addNewFabula :: Container -> (Fabula,Container)
addNewFabula Container{fabKeyCounter=c, table=t} = 
    Container{fabKeyCounter=c+1, table=Map.insert c (emptyFabula c) t}
    
addFabula    :: Fabula -> Container -> Container --throw out the original key of the fabula
-- just as extract all fabula in the original fabula, change their keys and insert to the container
addFabula Fabula{ evtKeyCounter=fekc, table=entityTable} Container{key=ck, fabKeyCounter=cfkc, table=fabulaTable} =
    Container{ key=ck, fabKeyCounter=cfkc+1, table=newTable}
    where
        newTable = Map.insert cfkc (Fabula cfkc fekc keyChangedEntityTable) fabulaTable
        keyChangedEntityTable = fmap () entityTable

removeFabula :: Fabula -> Container -> Container
removeFabula Fabula{key=k} c = removeFabulaWithKey k c
removeFabulaWithKey :: Int -> Container -> Container
removeFabulaWithKey k Container{keyCounter=c, table=t} =
    Container{keyCounter=c, table=Map.delete t}


------ Operations

--select :: FabulaEntity -> Fabula -> Maybe FabulaEntity
--selectEvent :: Event -> Fabula -> [FabulaEntity]
insert :: Event -> Fabula -> (Entity,Fabula)
remove :: Entity -> Fabula -> Fabula
addConseqOf :: Entity -> Fabula -> Either Entity Event -> Fabula
addCauseOf  :: Entity -> Fabula -> Either Entity Event -> Fabula
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


