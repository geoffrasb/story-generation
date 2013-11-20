module CommonSense.CommonSense
    ( Priming(..)
    , Prototype(..)
    , EventKnowledge(..)
    , Emotion(..)
) where

import qualified Data.Set as Set
import qualified Utils.Predicate as P

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


{-
class CognitiveSubject cs where
    recognize   :: (Image r) =>
        cs -> [r] -> (([r], r, [r]), cs)
    interpret   :: (Image r) =>
        [r] -> cs -> cs
    priming     :: (Image r1, Image r2) =>
        cs -> r1 -> (r2, cs)
    prototype   :: (Image r1, Image r2) =>
        cs -> r1 -> (r2, cs)
    reason      ::
    prove
    emotionalReaction
-}



class Image r where
    info :: r -> P.Predicate
instance Image P.Predicate where
    info p = p
{-
data State = EmptyState
           | State P.Predicate State
           deriving(Image)
-}

-- class Ontology o where

class GoalPursuing s where
    desire (valuation?)
    goalsetting
    emotion

class DoNarrating s where
    explain  :: (Image a) => s -> a -> a ->
    describe ::

class DoPerceiving s where
    perceive :: 

class DoInterpreting s where
    interpret :: s -> [Image] -> s

class HasEmotion s where
    react :: s -> Emotion -> s

class DoPrototyping p where
    recognize :: (Image a) => p -> [a] -> Maybe ([a], a, [a])
    prototype :: (Image a) => p -> a -> Maybe [a]

class DoPriming p where
    prime :: (Image a) => p -> a -> a
    --accept :: p -> Emotion -> p

class DoSubjectivePrototyping s where
    emoRecog :: (Image a) => s -> a -> a  -> Maybe Emotion
    emoProto :: (Image a) => s -> Emotion -> Maybe (a, a)
    emoComp  ::              s -> Emotion -> Emotion -> Maybe Emotion
    emoRedu  ::              s -> Emotion -> Maybe (Emotion, Emotion)

class DoSubjectivePriming s where --metapriming?

--- -- - - -- - ----- - -- - - --- - - -  - --- - - -- - --- - - - - - - - - - - - -- 

data Fabula r
    = Unit r [Fabula]
    | Pair Fabula Fabula [Fabula]
    deriving(Show,Eq)

data Emotion
    = Satisfied Desc Desc -- 1 prime 2, 1 been satisfied
    | Unsatisfied Desc
    | Unsatisfiable Desc Desc -- 2 make 1 unsatisfiable
    deriving(Show, Eq, Ord)
