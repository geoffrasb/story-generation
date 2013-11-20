import Utils

class TC1 tc where
    f :: tc -> Int
    f _ = 0


data D1 = D1 Int deriving(Show)
instance TC1 D1 where
    f (D1 a) = a
data D2 = D2 Int deriving(Show)
instance TC1 D2 where
data Fabula
    = Unit Int [Fabula]
    | Pair { pri::Fabula 
           , pos::Fabula 
           , link::[Fabula]
           }
    deriving(Show,Eq)
