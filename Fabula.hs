module Fabula (
    EntityInfo(..),
    Fabula(..),
    Entity,
    Plot
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Utils

-- if the data type of key changed, only these three lines should be modified.
data Key = Key Int deriving(Show,Eq,Ord)
nextKey (Key a) = Key (a+1)
bottomKey = Key 0

-- KeyGen should be singleton: keyGen
data KeyGen = KeyGen Key deriving(Show)
getKey (KeyGen a) = (a, KeyGen (nextKey a))
keyGen = KeyGen bottomKey

data EntityInfo = 
    EntityInfo {         
        key   :: Key,
        infab :: Maybe Fabula,
        next  :: [Fabula],
        prev  :: [Fabula]
    }
    deriving(Ord,Show)
instance Eq EntityInfo where
    EntityInfo ka fa _ _ == EntityInfo kb fb _ _ = ka==kb && fa=fb

data Fabula = 
    SingleEvent {
        evtPred :: Predicate
    } 
    | Fabula {
        evtPred    :: Predicate,
        table      :: Map.Map Key Entity
    }
    deriving (Ord, Show)

type Entity = (EntityInfo, Fabula)

evtInfo :: Fabula -> Predicate
evtInfo (SingleEvent p) = p
evtInfo (Fabula p _) = p

instance Eq Fabula where -- weak equality
    a == b = evtInfo a==evtInfo b


--data Plot = EmptyPlot | PlotPiece Predicate Plot
type Plot = [Fabula]




-- Fabula construction: every fabula should get a unique ID
newEntity :: Fabula -> KeyGen -> (Entity, KeyGen)
newEntity fab kg = ((EntityInfo{ key=nkey, infab=Nothing, next=[], prev=[]}, fab),
                    newkg)
                   where
                    (nkey,newkg) = getKey kg

moveIn  :: Entity -> Entity -> Entity
moveOut :: Entity -> Entity -> Entity



--operation on addr
{-
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
-}
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


