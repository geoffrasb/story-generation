


data Fabula = Unit Event [Fabula] -- wrapping fabulas
            | Pair Fabula Fabula [Fabula]
            deriving(Show,Eq)
data FabulaType = UF | PF | SF deriving (Show)
evt (Unit s) = s
evt (Pair s _) = s
evt (Set s _) = s
open (Just a) = a
open (Nothing) = error "tried to open Nothing"
--consOf :: Fabula -> [Fabula]

data Dir = LEFT | RIGHT deriving(Show)
type Event = String

-- op version 4
unit e = Unit e []
pair a b = Pair a b []
net = Pair (unit "A") (pair (unit "D") (unit "E")) [unit "C", Unit "B" [pair (unit "F") (unit "B")]]


-- op version 3
{- Construction should include 4 kinds
    unit source         unit derivation
                    X
    struture source     structure derivation
-}
-- 4 operations
{-
extend :: Fabula -> Fabula -> FabulaType Dir -> Fabula
extend _ _ UnitFabula = error "There's no unit extension"
extend f1 f2 (PairFabula dir) = 
    case dir of
        RIGHT -> Pair (evt f1) (f1,f2) -- maybe not (evt f1) but some way to denote the whole fabula
        LEFT  -> Pair (evt f1) (f2,f1)
extend f1 f2 SetFabula       =
    case f1 of
        Set e es  -> Set e (f2:es)
        otherwise -> Set (evt f1) [f1,f2]

pairExpand :: Fabula -> (Fabula,Fabula) -> Fabula
pairExpand (Unit e) pr = Pair e pr
pairExpand _ _         = error "Error: Expansion can only applied on Units"
setExpand :: Fabula -> [Fabula] -> Fabula
setExpand (Unit e) fs  = Set e fs
setExpand _ _          = error "Error: Expansion can only applied on Units"
-- abstract :: Fabula -> (Fabula -> Bool) -> Event -> Fabula
-}


{-
-- op version 2
exp' :: Fabula -> (Event, Event) -> Maybe Fabula
exp' f (e1,e2)
    | e1==evt f = Just $ update f e2 RIGHT
    | e2==evt f = Just $ update f e1 LEFT
    | otherwise = expd f (e1,e2)

-- macro version may be needed (link to a fabula)
update :: Fabula -> Event -> DIR -> Fabula
update f e dir =
    case dir of
        RIGHT -> Pair (evt f) (f, Unit e)
        LEFT  -> Pair (evt f) (Unit e, f)
expd :: Fabula -> (Event, Event) -> Maybe Fabula
expd (Unit e) (e1,e2) = Just $ Pair e (e1,e2)
-- expd (Pair _ _ _) _ _ = error "Tried to expand a Pair. this should be done in the previous step"
expd (Pair e (oe1,oe2)) (e1,e2) =
    case exp' oe1 (e1,e2) of
        Nothing -> 
            case exp' oe2 (e1,e2) of
                Nothing -> Nothing
                Just x  -> Pair e (oe1, x)
        Just x  -> Pair e (x, oe2)

-- op version 1
expu :: Fabula -> (Fabula,Fabula) -> PlotDist -> Fabula
expu (Unit e) pair pd = Pair e pair pd
expu (Pair _ _ _) _ _ = error "expu on Pair"

expp :: Fabula -> String -> (Fabula,Fabula) -> PlotDist -> Maybe Fabula
expp (Unit e) target expair expd
    | e == target = Just $ Pair e expair expd
    | otherwise   = Nothing
expp (Pair e (le,re) pd) target expair expd 
    | evt le == target = Just $ Pair e (open$expp le target expair expd, re) pd
    | evt re == target = Just $ Pair e (le, open$expp re target expair expd) pd
    | otherwise    = 
        case expp le target expair expd of
            Nothing -> 
                case expp re target expair expd of
                    Nothing -> Nothing
                    Just x  -> Just $ Pair e (le, x) pd
            Just x  -> Just $ Pair e (x, re) pd
-}

rules = 
    [ ("A","B")
    , ("A","C")
    , ("A","D")
    , ("A","E")
    , ("B","D")
    , ("B","E")
    , ("C","D")
    , ("C","E")
    , ("D","E")
    , ("F","B")
    , ("F","D")
    , ("F","E")
    ]


s1 = Unit "story";
