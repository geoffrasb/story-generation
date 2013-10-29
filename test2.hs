import Utils

{-
class TC1 tc where

class TC2 tc where
    f :: (TC1 tc) => tc -> Bool

data (TC1 a) =>TD a =  TD a
-}

f a = case
        case a of
            [] -> []
            x:xs -> xs
      of
        [] -> "<=1"
        otherwise -> ">1"
