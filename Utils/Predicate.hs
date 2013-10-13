module Utils.Predicate
    ( Predicate
    , atom
    , fromStr
    , fromTuple
) where

import Data.Char

--fromStr   :: String -> Maybe Predicate

atom :: String -> Predicate
atom s = if (isUpper$head s) then PredV s else Pred0 s

fromTuple :: (String, Int, [Predicate])
fromTuple (s, i, ps)
    | i == (length ps) = 
        case i of
            0         -> Just $ if (isUpper$head s) then PredV s else Pred0 s
            otherwise -> Just $ Pred s i ps
    | otherwise        = Nothing


toTuple   :: Predicate -> (String, Int, [Predicate])
toTuple (Pred0 s)     = (s, 0, [])
toTuple (PredV s)     = (s, 0, [])
toTuple (Pred s i ps) = (s, i, map toTuple ps)
                                         
data Predicate =                         
     Pred String Int [Predicate]
   | Pred0 String
   | PredV String
   deriving(Eq)

instance Show Predicate where
    show (Pred s _ args) = s ++ "("++ showlist ++ ")"
        where showlist = 
                 foldl (\acc p -> acc++", "++show p) (show $ head args) (tail args)
    show (Pred0 s) = s
    show (PredV s) = s
