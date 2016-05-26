module FileOperations where

import Data.Array ((:), concatMap)
import Data.Path (Path, ls)
import Prelude (bind)
allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child
