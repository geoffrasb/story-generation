


data Fabula = Unit Event [Fabula] -- wrapping fabulas
            | Pair Fabula Fabula [Fabula]
            deriving(Show,Eq)
open (Just a) = a
open (Nothing) = error "tried to open Nothing"

type Event = String

{-
    Current idea: Generate from an event
    Concepts:
        derivation: 
            + from Cms (priming, prototype)
            + using knowledge that related to emotion 
            + (proving) from intention
        updating:
            + abstraction: recognizing from current fabula structure
            + expansion: needed by proving 
            + reforming:
        intention:
            + implementing an abstract structure 
                (abstract structure may be derived by requirements of emotion)

-}

g :: Fabula -> Fabula
g f = recog f 
