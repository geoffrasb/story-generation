


data Fabula = Unit Event [Fabula] -- wrapping fabulas
            | Pair Fabula Fabula [Fabula]
            deriving(Show,Eq)
open (Just a) = a
open (Nothing) = error "tried to open Nothing"

type Event = String



derivate fab author = 
evaluate fab author
alternate fab author

g :: Fabula -> Author -> (Fabula, Author)



