module Fabula (
    getKey,
    keyGen,
    allEventMap,
    Plot,
    Event(..),
    Fabula
) where

{-
add type of causality, feature between two node
fabula node type can help planning

 becoming dense or reversely


-}

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
keyToNode :: Key -> IGraph.Node -- Int
keyToNode (Key a) = a

-- KeyGen should be singleton: keyGen
data KeyGen = KeyGen Key deriving(Show)
getKey (KeyGen a) = (a, KeyGen (nextKey a))
keyGen      = KeyGen bottomKey
allEventMap = Map.empty :: Map.Map Key Event


-- Interfaces, using keyGen and allEventMap

newEvent :: Predicate -> (Map.Map Key Event, KeyGen) -> (Event, (Map.Map Key Event, KeyGen))
newEvent p (em,kg) = (new_event, (newEm, newKg))
                where
                    new_event = Event newKey Nothing p
                    (newKey, newKg) = getKey kg
                    newEm = Map.alter (\e -> if e/=Nothing then error "Error: Conflict key when making new event."
                                              else Just new_event)
                                      newKey
                                      em

abstract :: Event -> Map.Map Key Event -> (Event, Map.Map Key Event)
abstract (Event k ie evt1) em = 
    (absEvent, new_em)
    where
        absEvent = AbsEvent k ie evt1 AG.empty Map.empty
        new_em   = Map.alter alterGuard k em
        alterGuard x = case x of
                        Nothing -> error "Error: Abstracting an event that does not exist."
                        Just y  -> if evt y==evt1 then Just absEvent
                                    else error "Error: The event to be abstracted different with eventMap."
abstract e@(AbsEvent _ _ _ _ _) em = (e, em)

deabstract :: Event -> Map.Map Key Event -> (Event, Map.Map Key Event)

putIn :: Event -> Event -> Map.Map Key Event -> (Event, Event, Map.Map Key Event)
--have to modify
putIn e1 (AbsEvent k2 iv2 ep2 fab2 impl) em =
    (new_e1, new_e2, new_em)
    where
        k1 = key e1
        new_e1 = case inevt e1 of
                    Nothing -> case e1 of
                                Event _ _ ep1               -> Event k1 (Just k2) ep1
                                AbsEvent _ _ ep1 fab1 impl1 -> AbsEvent k1 (Just k2) ep1 fab1 impl1
                    Just _  -> error "Error: Trying to put event that has inevt in another one."
        new_e2 = AbsEvent k2 iv2 ep2 (IGraph.insNode (keyToNode k1, e1) fab2) impl
        new_em = Map.alter (\_ -> Just new_e1) k1 $ Map.alter (\_ -> Just new_e2) k2 em
putIn _ (Event _ _ _) _ = 
    error "Error-Advice: Make the event abstract before putting any event into it."

{-
takeOut :: (Either Key Event) -> Event -> (Event,Event)
takeOut (Left k) (AbsEvent d
-}

{-
update
link
deabstract :: Entity -> Fabula -> Fabula
-}


type Plot = [Event]

data Event = 
      Event {
          key   :: Key,
          inevt :: Maybe Key,
          evt   :: Predicate }
    | AbsEvent{
          key   :: Key,
          inevt :: Maybe Key,
          evt   :: Predicate,
          fabula:: Fabula,
          impl  :: Map.Map Key Key}
instance Show Event where
    show Event{key=k, evt=e, inevt=ene} = 
        "Event: key = "++(show k)++", evt = "++(show e)++", inevt = "++(show ene)
    show AbsEvent{key=k, evt=e, inevt=ene} =
        "AbsEvent: key = "++(show k)++", evt = "++(show e)++", inevt = "++(show ene)
instance Eq Event where
    e1 == e2 = key e1 == key e2

sameEvents :: Event -> Event -> Bool
sameEvents e1 e2 = evt e1 == evt e2

type Fabula = AG.AlgeGraph Event ()


