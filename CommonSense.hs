module CommonSense
    ( Priming(..)
    , Enablement(..)
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
        Enablement
        Prototype
        Event knowledge
        Ontology

-}

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

data Enablement = Enablement 
    { evt     :: Event
    , precond :: Set.Set Predicate
    , conseq  :: Set.Set Predicate
    } deriving (Show)

-- mental events?
data Emotion = Expect Priming
             | Satisfied Priming
             | QuasiSat Priming (Set.Set Event)
             | Confuse (Either (Priming, Set.Set Event) (Enablement, Set.Set Predicate)) 
             | Default Enablement
             | Unaware Enablement
