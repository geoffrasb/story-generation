module Inductive.Fabula
    ( Fabula(..)
    , anony_evt_by
    , syuzhet
    , expand
    , extend
    , shrink
    , evt
    , alter_evt
    , alter_pd
    , alter_pair
    ) where

import Utils

{- note:
    * semantics of extenstion and expansion (use only one or two of them?)
    * syuzhet's order
-}

-- macros
    {-
    -}

-- problem: the target of expansion and extension



add_cons :: Fabula -> Maybe Event -> Fabula -> Fabula
add_cons fab1@(Unit e) ae fab2 = 
    case ae of 
        Nothing    -> Pair (anony_evt_by e) LR (fab1, fab2)
        Just label -> Pair label LR (fab1, fab2)
add_cons fab1@(Pair e pd (lf, rf)) ae fab2
    | pd == RL || pd == LL = Pair e pd (add_cons lf ae fab2, rf)
    | pd == LR || pd == RR = Pair e pd (lf, add_cons rf ae fab2)

add_caus :: Fabula -> Maybe Event -> Fabula -> Fabula
add_caus fab1@(Unit e) ae fab2 =
    case ae of
        Nothing    -> Pair (anony_evt_by e) LR (fab2, fab1)
        Just label -> Pair label LR (fab2, fab1)
add_caus fab1@(Pair e pd (lf, rf)) ae fab2
    | pd == RL || pd == LL = 
    | pd == LR || pd == RR =


-- primitive operations
    {-
    Unit :: Event -> Fabula
    Pair :: Event -> PlotDist -> (Fabula, Fabula) -> Fabula

    anony_evt_by :: Fabula -> Event
    syuzhet :: Fabula -> Syuzhet
    expand :: Fabula -> PlotDist -> (Fabula, Fabula) -> Maybe Fabula
    extend :: Fabula -> Fabula -> PlotDist -> Event -> Fabula
    shrink :: Fabula -> Maybe Fabula
    evt :: Fabula -> Event
    alter_evt :: Fabula -> Event -> Fabula
    alter_pd :: Fabula -> PlotDist -> Maybe Fabula
    alter_pair :: Fabula -> (Fabula,Fabula) -> Maybe Fabula
    -}

anony_evt_by :: Fabula -> Event
anony_evt_by e = Pred "anonymous_by" 1 [show $ evt e]

syuzhet :: Fabula -> Syuzhet
syuzhet (Unit e) = [e]
syuzhet (Pair e0 pd (e1,e2)) = 
    case pd of
        LL -> syuzhet e1 
        RR -> syuzhet e2
        LR -> (syuzhet e1) ++ (syuzhet e2)
        RL -> (syuzhet e2) ++ (syuzhet e1)

expand :: Fabula -> PlotDist -> (Fabula, Fabula) -> Maybe Fabula
expand (Unit e) pd pair = Just $ Pair e pd pair
expand (Pair _ _ _) _ _ = Nothing

extend :: Fabula -> Fabula -> PlotDist -> Event -> Fabula
extend fab1 fab2 pd newWrap = Pair newWrap pd (fab1, fab2)

shrink :: Fabula -> Maybe Fabula
shrink (Unit _)     = Nothing
shrink (Pair e _ _) = Just $ Unit e

evt :: Fabula -> Event
evt (Unit e) = e
evt (Pair e _ _) = e

alter_evt :: Fabula -> Event -> Fabula
alter_evt (Unit _) ne = Unit ne
alter_evt (Pair _ pd pair) ne = Pair ne pd pair

alter_pd :: Fabula -> PlotDist -> Maybe Fabula
alter_pd (Unit _) _          = Nothing
alter_pd (Pair e _ pair) npd = Just $ Pair e npd pair

alter_pair :: Fabula -> (Fabula,Fabula) -> Maybe Fabula
alter_pair (Unit _) _          = Nothing
alter_pair (Pair e pd _) npair = Just $ Pair e pd npair


data Fabula =
      Unit Event
    | Pair Event PlotDist (Fabula,Fabula)
    deriving(Show,Eq,Ord)

data PlotDist = LL | LR | RL | RR deriving(Show, Eq)
type Event = Predicate
type Syuzhet = [Event]


