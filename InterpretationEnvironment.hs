module InterpretationEnvironment (
) where

import Utils
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Fabula as Fabula


-- supposed interface for story generation

interpret and check the mental result





-- objects
{-
    events are described in WorldK (?
-}
data WorldK = WorldK Predicate
data PrimingRule = PrimingRule Primer Primee

{-
    WorldKs stored in map or set? 
    Consider about using Map is that knowledges have revisions. But revisions may also constructed by priming rules.
-}
data IE = IE {
    worldKs :: [WorldK],
    priming :: 
    contexts ::
}


-- axiomatic Events
    --maybe even these events are not natured
    --what events are?
-- types of mental events:
perceive 
recognize
ignore
expect --part of emotion elements ?
vacuum



{-
  The story interpretation started from some motivation, and it would continued by the interaction result between reader and the plot.
  In other words, bad story could lead to abandon.
  How to start?
    motivation from coplot
  How to stop?
    make a interruption
    or the interruption comes from mind
-}
interpret :: ((IE, Fabula.Plot) -> Fabula.Event -> (IE, Fabula.Plot)) -> (IE, [Fabula.Event]) -> Fabula.Plot -> (IE,[Fabula.Event]) 
interpret foldf (ie,coplot) EmptyPlot               = (ie,coplot)
interpret foldf (ie,coplot) (PlotPiece ev restPlot) = interpret foldf (foldf (ie,coplot) ev) restPlot 
-- an alias
reducePlot :: ((IE, Fabula.Plot) -> Fabula.Event -> (IE, Fabula.Plot)) -> (IE, [Fabula.Event]) -> Fabula.Plot -> (IE,[Fabula.Event]) 
reducePlot = interpret


{-
  Consider the folding function (named: perceive)
  Easiest task of folding: Just let the coplot growth, don't watch on it.
-}
{- event triggering
  Mental events are triggered by ...
  - Perception triggered by some motivations
  - recognition 

-}
perceiveFunction :: (IE, Fabula.Plot) -> Fabula.Event -> (IE, Fabula.Plot)
perceive (ie,coplot) ev =  

-- the combination: com perceptionFunction (IE,[mot]) plot
-- Reader read Plot


imageOf :: Fabula.Plot -> Fabula.Event -- equals to recognize
priming :: Fabula.Event -> 
prove :: Fabula.Event -> Fabula.Event -> 
--reasoning?
--learning?

-------------- new thought
{-
    -- event knowledge is constructed by priming rules and world knowledge 
data EventImage = EventImage {  -- or, mental event, recognized event, primer...
    event :: String,
    info :: [String],
    time :: Int                 -- or (either Int ?)
}
perceive :: Fabula.Event -> EventImage
eventRecognize :: [EventImage] ->  EventImage -- perceived plot including the story's and reader himselves
objectRecognize :: [WorldK] -> WorldK
prime :: EventImage -> Primee
memory :: EventImage -> WorldK
memory EventImage{ event=e, info=i, time=t} = (e, (length i)+1, t:info)
data Context =
    Context {
        primingRules :: (Set.Set PrimingRules),
        worldK :: (Set.Set WorldK),
        characters :: (Set.Set Character),
        fabula :: Fabula.Fabula
    }
empty :: Context
empty = Context {knowledge=Set.empty, characters=Set.empty, fabula=Fabula.empty }
-}
