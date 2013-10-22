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
