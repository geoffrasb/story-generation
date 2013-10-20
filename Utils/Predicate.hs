module Utils.Predicate
    ( Predicate(..),
) where
import qualified Data.Map as Map
import Data.Char

--fromStr   :: String -> Maybe Predicate

{-
unify :: Predicate -> Predicate -> [Predicate]
unify p1 p2 = unify' p1 p2 (Just Map.empty)

unify' :: Predicate -> Predicate -> Maybe (Map.Map Predicate [Predicate])
unify' p1 p2 varTable =
    case (p1, p2) of
        (Atom s1, Atom s2) -> 
            if s1==s2 then Just Map.empty else Nothing
        (Var s1, Var s2) ->
        (Var s1, x) -> 
        (x, Var s2) ->
        (Pred s1 i1 ps1, Pred s2 i2 ps2) ->
            if s1/=s2 || i1/=i2 then 
                Nothing
            else
                foldl (\acc (pa,pb) -> acc >>= (\x -> unify' pa pb x))
                      (Just Map.empty)
                      (zip ps1 ps2)
        otherwise ->
            Nothing
-}        

atom :: String -> Predicate
atom s = if (isUpper$head s) then Var s Nothing else Atom s

{-
fromTuple :: (String, Int, [Predicate]) -> Maybe Predicate
fromTuple (s, i, ps)
    | i == (length ps) = 
        case i of
            0         -> Just $ if (isUpper$head s) then Var s Nothing else Atom s
            otherwise -> Just $ Pred s i ps
    | otherwise        = Nothing
-}

{-
tuple   :: Predicate -> (String, Int, [Predicate])
tuple (Atom s)      = (s, 0, [])
tuple (Var s _)     = (s, 0, [])
tuple (Pred s i ps) = (s, i, map tuple ps)
-}
                                         
data Predicate =                         
     Pred String Int [Predicate]
   | Atom String
   | Var String (Maybe Predicate)
   deriving(Eq)

instance Show Predicate where
    show (Pred s _ args) = s ++ "("++ showlist ++ ")"
        where showlist = 
                 foldl (\acc p -> acc++", "++show p) (show $ head args) (tail args)
    show (Atom s) = s
    show (Var s v) = s++"="++(show v)
