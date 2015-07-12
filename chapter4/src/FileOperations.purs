module FileOperations where

import Prelude

import Data.Path
import Data.List

allFiles :: Path -> List Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> List Path
allFiles' file = file : do
  child <- ls file
  allFiles' child
