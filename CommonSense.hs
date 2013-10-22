module CommonSense
    ( Priming(..)
    , Prototype(..)
    , EventKnowledge(..)
    , Emotion(..)
) where

import Utils.Predicate
import Event
import qualified Data.Set as Set

{-
    Works to do:
        define class Ontology
    CommonSense: 
        Priming
        Event knowledge
        Prototype
        Ontology
        Emotional knowledge
            we get this defaultly or though interpretation(simulation)

what is conflict?
    fail of fast proving (all in Cms domain, no computation(?))
what is the reason of an event? (reasonable event?)
    reason of doing something
reasonability and emotion?
    evaluation on people's action
-}

{- example
state Cms: under(chair, table)
fact: under(table, chair) >< under(char, table) (under(A,B) >< under(B,A), A=/=B)
    how do we rationalize this fact?
        We find an reasonable event that leads to this fact

event Cms: hit(A,B) -> cry(B)
fact: hit(A,B) => laugh(B) , laugh(X) >< cry(B)
    how do we rationalize this event sequence?
        we search for reasonable link between them

-}

class State where

class Event where

-- commonsense is necessarily related to emotion
--  contrast with cognition(?

-- recognization





class Ontology a where

data EventKnowledge = About 
    { evt :: Event 
    , precond :: (Set.Set Predicate)
    , conseq  :: (Set.Set Predicate)
    } 

-- Events could be expanded by their prototypes.
data Prototype = Prototype Event (Set.Set [Event])

-- We expect numbers of events after an event was happened.
data Priming = Priming Event (Set.Set Event)

-- mental events?
data Emotion = Expect Priming
             | Satisfied Priming
             | QuasiSat Priming (Set.Set Event)
             | Confuse (Either (Priming, Set.Set Event) (Enablement, Set.Set Predicate)) 
             | Default Enablement
             | Unaware Enablement
