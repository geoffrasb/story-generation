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

    Conflict: fail of fast proving
-}

-- commonsense is necessarily related to emotion
--  contrast with cognition(?

recognize :: (Recognizable r, CognitiveSubject cs) => 
    r -> cs ->

class CognitiveSubject cs where


class Recognizable r where

class (Recognizable e) => Event e where

class (Recognizable e) => State s where

-- class Ontology o where

data (Event e, State s) => EventKnowledge e s
    = About 
    { evt :: e
    , precond :: (Set.Set s)
    , conseq  :: (Set.Set s)
    } deriving(Show, Eq, Ord)

data (Recognizable e) => Prototype e 
    = Prototype e (Set.Set e)
    deriving(Show, Eq, Ord)

data (Recognizable e) => Priming e 
    = Priming e (Set.Set e)
    deriving(Show, Eq, Ord)

-- mental events?
data (Event e, State s) => Emotion e s 
    = Expect (Priming e)
    | Satisfied (Priming e)
    | QuasiSat (Priming e) (Set.Set e)
    | Confuse (Either ((Priming e), Set.Set e) (EventKnowledge e s, Set.Set s)) 
    | Default (EventKnowledge e s)
    | Unaware (EventKnowledge e s)
    deriving(Show, Eq, Ord)
