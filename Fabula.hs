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
        inEnt :: Maybe Entity, -- in entity
        next  :: [Fabula],
        prev  :: [Fabula]
    }
    deriving(Ord,Show)
instance Eq EntityInfo where
    (EntityInfo ka fa _ _) == (EntityInfo kb fb _ _) = ka==kb && fa==fb

data Fabula = 
    SingleEvent {
        evtPred :: Predicate
    } 
    | Fabula {
        evtPred    :: Predicate,
        table      :: Map.Map Key Entity
    }
    deriving (Ord, Show)
instance Eq Fabula where -- weak equality
    a == b = evtInfo a==evtInfo b
{-
instance Functor Fabula where
    fmap f e@(SingleEvent e) = f e
    fmap f (Fabula evt t) = f $ Fabula evt (fmap f t)
    -}

type Entity = (EntityInfo, Fabula)

evtInfo :: Fabula -> Predicate
evtInfo (SingleEvent p) = p
evtInfo (Fabula p _) = p



--data Plot = EmptyPlot | PlotPiece Predicate Plot
type Plot = [Fabula]




-- Fabula construction: every fabula should get a unique ID
newEntity :: Fabula -> KeyGen -> (Entity, KeyGen)
newEntity fab kg = ((EntityInfo{ key=nkey, inEnt=Nothing, next=[], prev=[]}, fab),
                    newkg)
                   where
                    (nkey,newkg) = getKey kg


{-
    operations below may be constructed from fmap...?


-}


attach  :: Entity -> Entity -> Maybe Entity
-- Tag that ent1 is wrapped in ent2. 
--  If ent1 is already wrapped in an entity, then it comes out with Nothing.
--  If not, it returns the new ent1.
attach ((EntityInfo _ (Just _) _ _), _) _ = Nothing -- means ent1 is already a child of an entity.
attach ((EntityInfo key1 Nothing n1 p1), fab1) ent2 =
    Just ((EntityInfo key1 (Just ent2) n1 p1), fab1)


detach :: Entity -> (Entity, Maybe Entity)
-- Free the ent1.
--  If ent1 is wrapped in ent2, then it returns (ent1 without parent, ent2 without ent1)
--  If ent1 has no parent, then (original ent1, Nothing)
detach ent1@((EntityInfo _ Nothing _ _), _) = (ent1, Nothing)
detach ((EntityInfo key1 (Just inEnt1) n1 p1), fab1) =
    (((EntityInfo key1 Nothing n1 p1), fab1), Just newOutterEnt)
    where
        newOutterEnt = case inEnt1 of
                        (_, (SingleEvent _)) -> 
                            error "Error: in detach, the first argument attached to a SingleEvent."
                        (ei, (Fabula evt table)) ->
                            (ei, Fabula evt $ Map.delete key1 table)


updateOutterEntity :: Entity -> Maybe Entity
-- Returns the updated entity which used to have the key that links to 
--      the old version of the input entity.
--  Nothing if the entity has no parent.
updateOutterEntity ent1@((EntityInfo key1 inEnt1 n1 p1), fab1) = 
    case inEnt1 of
        Nothing -> Nothing
        Just (ei2, fab2) -> Just (ei2, updatedFab2)
            where
                updatedFab2 = case fab2 of
                                --inEnt1 will definitely be updated with ent1
                                (SingleEvent evt) -> 
                                    Fabula evt $ Map.fromList [(key1, ent1)]
                                (Fabula evt table) ->
                                    Fabula evt $ Map.alter (\_->Just ent1) key1 table



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


