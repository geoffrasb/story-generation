module InterpretationEnvironment (
) where

import Utils
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Fabula as Fabula



type Predicate = (String, Int, [String]) -- pred, arity, [parameter]

data PrimingRule = PrimingRule Primer Primee
                -- event knowledge is constructed by priming rules and world knowledge 
type WorldK = Predicate

data EventImage = EventImage {  -- or, mental event, recognized event, primer...
    event :: String,
    info :: [String],
    time :: Int                 -- or (either Int ?)
}
data Primee


perceive :: Fabula.Event -> EventImage
eventRecognize :: [EventImage] ->  EventImage -- perceived plot including the story's and reader himselves
objectRecognize :: [WorldK] -> WorldK
prime :: EventImage -> Primee
{- funcitonality of priming:
 -  causation
 -  enablement
 -  knowledge <-> event link
 -  cause of reasoning
 -}

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



interpret :: Context -> (Context -> Fabula.Plot) -> (Context, InterpretationReport)
interpret c f = interpret' c (f c)

interpret' :: Context -> Fabula.Plot -> (Context, InterpretationReport)
interpret' (Context{ }) [] =
interpret' (Context{ }) f:p =


--prove :: Context -> (Context -> (Fabula.FabulaElement, Fabula.FabulaElement)) -> Context




--abduct :: FabulaElement -> FabulaElement
--context operations v
--
--context operations ^

