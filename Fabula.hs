module Fabula (
    Event(..),
    FabulaElement,
    Plot,
    Fabula,
    empty
) where

-- import qualified Data.Graph as Graph
-- import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map

data Event = 
    Event {             -- the representation should be considered
        name :: String,
        characters :: [String]
    } deriving (Eq, Ord)

type FabulaElement = Event

type Plot = [Event]

data Fabula = Fabula {
    elems :: Set.Set Event,
    links :: Map.Map Event ([Event], [Event]) --(pri, post)
}

empty = Set.empty :: Fabula


