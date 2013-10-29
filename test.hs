


data Fabula = Unit Event [Fabula] -- wrapping fabulas
            | Pair Fabula Fabula [Fabula]
            deriving(Show,Eq)
open (Just a) = a
open (Nothing) = error "tried to open Nothing"

type Event = String

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

data EditAction 
    = Derivation -- doing priming, 
    | Evaluation -- interpretation using EventKnowledge
    | Alternation -- alternate fabula accordings to evaluation result
    deriving(Eq,Show)

derivate fab author = 
evaluate fab author
alternate fab author

g :: Fabula -> Author -> (Fabula, Author)
g f author = 
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
    where
        apply f (a,b) = f a b




