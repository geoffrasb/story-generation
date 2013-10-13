import Utils
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Graph.Inductive.Graph as IGraph

_f a 
    | a>1 || a==0 = True
    | otherwise   = False
