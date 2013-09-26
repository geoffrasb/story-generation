module Fabula (
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Utils

type Key = Int


data EntityInfo = 
    EntityInfo {          --problem: should I know where the entity belongs to from the entity? yes
        addr  :: [Key],   --[outter to inner] 
        next  :: [Fabula],
        prev  :: [Fabula]
    }
    deriving(Eq,Ord,Show)

data Fabula = 
    SingleEvent {
        evtPred :: Predicate
    } 
    | Fabula {
        evtPred    :: Predicate,
        table      :: Map.Map Int Entity,
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


--operation on addr
pushAddr :: Key -> Entity -> Entity
pushAddr k ((EntityInfo addr n p), fab) = (EntityInfo (k:addr) n p, fab)
popAddr  :: Entity -> (Key,Entity)
popAddr ((EntityInfo (k:addrs) n p), fab) = (k, (EntityInfo addrs n p, fab))

insert :: Either Fabula Entity -> Fabula -> (Entity,Fabula)
insert f_or_e fabula2@(Fabula evt2 table2 keycntr2) = 
    (newEntity, newFabula2)
    where
        newEntity = (newEntityInfo, finallyAddedFabula)
        newFabula2 = Fabula evt2 (Map.insert keycntr2 newEntity table2) (keycntr2+1)
        (newEntityInfo, insertFabula) = 
            case f_or_e of
                (Left fab) -> 
                    (EntityInfo [keycntr2] [] [],    fab)
                (Right ((EntityInfo addr n p), fab)) ->
                    (EntityInfo (keycntr2:addr) n p, fab)
                    -- current method of updating the addr may be wrong
        finallyAddedFabula = 
            case insertFabula of
                (SingleEvent _) -> 
                    insertFabula
                (Fabula evt1 table1 keycntr1) ->
                    Fabula evt1 (fmap (pushAddr keycntr2) table1) keycntr1
                    -- current method of updating the addr may be wrong
        
insert _ (SingleEvent _) = error "Cannot insert a Fabula into a SingleEvent."

{-
remove :: [Key] -> Fabula -> Fabula
update :: Entity -> Fabula -> Fabula
conseqLink :: (Entity, Entity) -> Fabula -> Fabula
causeLink  :: (Entity, Entity) -> Fabula -> Fabula
conseqOf :: Entity -> [Entity]
causeOf  :: Entity -> [Entity]
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


