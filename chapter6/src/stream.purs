module Stream where

import Prelude (show, (<>))
import Data.Maybe
import Data.Monoid (class Monoid, mempty)


-- 6.8 Multi Parameter Type Classes

class Stream list element where
  uncons :: list -> Maybe { head :: element, tail :: list }

instance streamArray :: Stream (Array a) a where
  uncons = Data.Array.uncons

instance streamString :: Stream String Char where
  uncons = Data.String.uncons


unconsResult :: Maybe {head :: Char, tail:: String}
unconsResult =  uncons "123"

foldStream :: forall l e m. (Stream l e, Monoid m) => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail



foldResult :: String
foldResult = foldStream (\x -> x)  ["1"]

foldResult2 :: String
foldResult2 = foldStream toString  [1,2,3]

foldResult3 :: Array Char
foldResult3 = foldStream toSingletonArray  "abc"

toSingletonArray :: Char -> Array Char
toSingletonArray c = [c]

toString :: Int -> String
toString = show

-- foldStream (\x -> [0,x] ) [1,2,3]
