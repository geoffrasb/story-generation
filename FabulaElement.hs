module FabulaElement (
    FabulaElement(..)
) where

import qualified Character as C


data FabulaElement = 
    FabulaElement {
        tag :: [String],
        characters :: [C.Character],
        action :: String,
        precondition :: Prop, 
        poscondition :: Prop,
        causedBy :: [FabulaElement],
        cause :: [FabulaElement]
    }
    
