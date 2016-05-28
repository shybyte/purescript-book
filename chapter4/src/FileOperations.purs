module FileOperations where

import Data.Array (filter, (:), concatMap)
import Data.Array.Unsafe (tail, head)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Path (filename, size, isDirectory, Path, ls, root)
import Prelude ((>>>), not, bind, ($), (<), (>), (==))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child


-- 1. Write a function onlyFiles which returns all files (not directories) in all subdirectories of a directory.

isFile :: Path -> Boolean
isFile = isDirectory >>> not

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter isFile


-- 2. Write a fold to determine the largest and smallest files in the filesystem.

smallestFile :: Array Path -> Maybe Path
smallestFile = selectElement smallerFile

largestFile :: Array Path -> Maybe Path
largestFile = selectElement largerFile

selectElement :: forall a. (a -> a -> a) -> Array a -> Maybe a
selectElement _ [] = Nothing
selectElement _ [file] = Just file
selectElement selector files =  Just $ foldl selector (head files) (tail files)


smallerFile :: Path -> Path -> Path
smallerFile path1 path2 = if size1 < size2 then path1 else path2
  where
    size1 = fromMaybe 0 (size path1)
    size2 = fromMaybe 0 (size path2)


largerFile :: Path -> Path -> Path
largerFile path1 path2 = if size1 > size2 then path1 else path2
  where
    size1 = fromMaybe 0 (size path1)
    size2 = fromMaybe 0 (size path2)


-- 3. Write a function whereIs to search for a file by name.

whereIs :: String -> Maybe Path
whereIs fn = Data.Array.head $ whereIs' root fn

whereIs' :: Path -> String -> Array Path
whereIs' path fn = do
  child <- ls path
  if filename child == fn then [path] else whereIs' child fn
