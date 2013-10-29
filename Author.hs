module Author (

) where

import qualified CommonSense as Cms
import qualified Event as E
import qualified Data.Set as Set
import qualified Data.Map as Map


-- ontology has to be added
data Author = Author
    { eventKnoledges :: Map.Map E.Event Cms.EventKnowledge
    , prototypes     :: Map.Map E.Event Cms.Prototype
    , primingRules   :: Map.Map E.Event Cms.Priming
    } 

-- author is also an instance of Cms.CognitiveSubject
class Author a where
    evtkMap :: (Cms.CognitiveSubject a, Cms.Event e,Cms.State s) => 
        a -> Map.Map e (Cms.EventKnowledge e s)
    protoMap :: (Cms.CognitiveSubject a, Cms.Event e) => 
        a -> Map.Map e (Cms.Prototype e)
    primingMap :: (Cms.CognitiveSubject a, Cms.Event e) => 
        a -> Map.Map e (Cms.Priming e)

