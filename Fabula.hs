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



-- new data structure design

data Event = Event Predicate deriving(Show,Eq)

data BrightTree a = BrightNode {
    key :: Key,
    lab :: a,
    up  :: Maybe (BrightTree a),
    down:: Map.Map Key (BrightTree a),
    next:: [BrightTree a],
    prev:: [BrightTree a]
    }
instance Eq (BrightTree a) where
    BrightNode{key=k1, lab=l1} == BrightNode{key=k2, lab=l2} = k1==k2 && l1==l2
instance Functor BrightTree where
    fmap f (BrightNode key lab up down next prev) = 
        BrightNode key (f lab) up (fmap (\bt->fmap f bt) down) next prev

type Fabula = BrightTree Event




type Plot = [Fabula]

newFabula :: Event -> KeyGen -> (Fabula, KeyGen)
newFabula evt kg = (BrightNode nkey evt Nothing Map.empty [] [], newkg)
                   where
                    (nkey,newkg) = getKey kg




attach  :: Fabula -> Fabula -> Maybe Fabula
-- Tag that ent1 is wrapped in ent2. 
--  If ent1 is already wrapped in an entity, then it comes out with Nothing.
--  If not, it returns the new ent1.
attach (BrightNode{up=Just _) _ = Nothing -- means ent1 is already a child of an entity.
attach (BrightNode key1 evt1 _ d1 n1 p1) fab2 = 
    Just $ BrightNode key1 evt1 fab2 d1 n1 p1


detach :: Fabula -> (Fabula, Maybe Fabula)
-- Free the ent1.
--  If ent1 is wrapped in ent2, then it returns (ent1 without parent, ent2 without ent1)
--  If ent1 has no parent, then (original ent1, Nothing)
detach fab1@BrightNode{up=Nothing} = (fab1, Nothing)
detach (BrightNode key1 evt1 (BrightNode k2 e2 u2 d2 n2 p2) d1 n1 p1) =
    (BrightNode key1 evt1 Nothing d1 n1 p1, Just newOutterFab)
    where
        newOutterFab = BrightNode k2 e2 u2 (Map.delete key1 d2) n2 p2


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





{-
putIn
takeOut
update
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


