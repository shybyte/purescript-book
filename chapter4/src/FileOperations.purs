module FileOperations where

import Prelude

import Data.Path
import Data.Array
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Foldable

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles path = filter isFile (allFiles path)

isFile = not <<< isDirectory

largestFile :: Path -> Maybe Path
largestFile path = foldl foldF Nothing (onlyFiles path)
  where
    foldF :: Maybe Path -> Path -> Maybe Path
    foldF (Just biggestFile) file  =
      if file `isBigger` biggestFile then
        Just file
      else
        Just biggestFile
    foldF Nothing file  = Just file

    isBigger :: Path -> Path -> Boolean
    isBigger file1 file2 = sizeOfFile file1 > sizeOfFile file2
      where
        sizeOfFile = fromJust <<< size


whereIs :: String -> Maybe Path
whereIs fileNameNeedle = whereIs' root
  where
    whereIs' :: Path -> Maybe Path
    whereIs' directory =
      if isDirectory directory then
        if containsFileDirectly directory then
          Just directory
        else
          let
            subfolderResults :: Maybe (Maybe Path)
            subfolderResults = find isJust (map whereIs' (ls directory))
          in
            fromMaybe Nothing subfolderResults
      else
        Nothing

    containsFileDirectly :: Path -> Boolean
    containsFileDirectly path = any (hasFileName) (ls path)

    hasFileName path = filename path == fileNameNeedle







