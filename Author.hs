module Author (

) where
{- About this module:
    Defined the class of author and an implimentation.
-}

import qualified CommonSense as Cms
import qualified Event as E
import qualified Data.Set as Set
import qualified Data.Map as Map

{-
    applications of makeStory:
        improvisation storytelling
        embellishment storytelling
        tutoring
        try to convey some information
-}

{-
    Current idea: Generate from an event/prototype
    actions of editing: even these actions would compete
        physical:
            write
            delete
            alternate
        mental:
            goal "editing"
            evaluation
    Concepts:
        derivation: 
            + from Cms (priming, prototype)
            + using knowledge that related to emotion 
            + emotional planning
        proving: 
            + from goals of intention, this including fabula alternation
        updating:
            + abstraction: recognizing from current fabula structure
            + expansion: needed by proving 
        intention:
            + implementing an abstract structure 
                (abstract structure may be derived by requirements of emotion)
        verification:
            ?
-}

makeStory :: a -> Goal
makeStory f author = 
    case
        case selectEditAction author f of
            Derivation ->
                derivate f author
            Evaluation ->
                evaluate f author
            Alternation ->
                alternate f author
    of
        Left x  -> x
        Right (new_f, new_author) -> g new_f new_author

data EditAction 
    = Derivation -- doing priming, 
    | Evaluation -- interpretation using EventKnowledge
    | Alternation -- alternate fabula accordings to evaluation result
    deriving(Eq,Show)

{- CognitiveSubjects are basicly readers, authors have more knowledge/abilities.
    An author can explicitly list out knowledge about event and state proposition.
-}
class (CognitiveSubject cs) => Author cs where
    eventKMap       :: (Event e, State s) => cs -> Map.Map e (EventKnowledge e s)
    eventProtoMap   :: (Event e) => cs -> Map.Map e (Prototype e)
    stateProtoMap   :: (State s) => cs -> Map.Map s (Prototype s)
    eventPrimingMap :: (Event e) => cs -> Map.Map e (Priming e)
    statePrimingMap :: (State s) => cs -> Map.Map s (Priming s)
