module Inductive.Fabula
    ( Fabula(..)
    , Story(..)
    ) where

import Utils
import Event

{- Hypothesis:
    Fabula construction : data Fabula
    Syuzhet : reordering of the main stem of fabulas
-}

data Story = Story
    { fabula  :: Fabula
    , stem    :: Fabula -> Syuzhet
    , syuzhet :: Syuzhet -> Syuzhet
    }

data Fabula =
      Unit Event [Fabula]
    | Pair Fabula Fabula [Fabula]
    deriving(Show,Eq)

type Syuzhet = [Event]


