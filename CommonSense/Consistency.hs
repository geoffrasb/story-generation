module CommonSense.Consistency
    (
) where

import qualified Utils.Predicate as P
import qualified Event as E

data ConflictPair = ConflictPair E.Event E.Event
