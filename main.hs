import Data.Graph as Graph





-- fabula elements instances
data InternalElement = Cognition
                     | Emotion
                     | Feeling
                     | Belief 
data Outcome = Outcome OutcomeParameter
data Perception = Saw
                | Heard
                | Smelt
                | Tasted
                | Felt --physically
data Event = Event OutcomeParameter
data Action
data Goal
-- / fabula elements instances
