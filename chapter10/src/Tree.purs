module Tree where

-- 10.19 Serializing Address Book Entries - 5.Exercise
-- (De)serialize a binary tree.

import Data.Either (Either(Left, Right))
import Prelude (class Show, ($), bind, return, (<$>), (<*>), show, (<>))
import Data.Foreign (ForeignError(JSONError), F, toForeign)
import Data.Foreign.Class (class IsForeign, readJSON, readProp, read)
import Data.JSON (stringify)



data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance treeShow :: (Show a) => Show (Tree a) where
  show (Leaf a) = "Leaf " <> show a
  show (Branch left right) = "(" <> show left <> ", " <> show right <> ")"


newtype TreeJSON a = TreeJSON {
  value :: Array a,
  left :: Array (TreeJSON a),
  right :: Array (TreeJSON a)
}

instance treeJsonShow :: (Show a) => Show (TreeJSON a) where
  show (TreeJSON {value: [x], left: [], left: []}) =
    "Leaf " <> show x
  show (TreeJSON {value: [], left: [left], right: [right]}) =
    "(" <> show left <> ", " <> show right <> ")"
  show otherwise = "Invalid Tree"

toTreeJSON :: forall a. Tree a -> TreeJSON a
toTreeJSON (Leaf a) =
  TreeJSON {
    value: [a],
    left: [],
    right: []
  }
toTreeJSON (Branch left right) =
  TreeJSON {
    value: [],
    left: [toTreeJSON left],
    right: [toTreeJSON right]
  }


fromTreeJSON :: forall a. TreeJSON a -> F (Tree a)
fromTreeJSON (TreeJSON {value: [a], left: [], left: []}) =
  Right $ Leaf a
fromTreeJSON (TreeJSON {value: [], left: [left], right: [right]}) =
  (Branch <$> (fromTreeJSON left)) <*> (fromTreeJSON right)
fromTreeJSON otherwise =
  Left (JSONError "Bad JSON Tree")


instance treeJsonIsForeign :: (IsForeign a) => IsForeign (TreeJSON a) where
  read json = do
    value <- readProp "value" json
    left <- readProp "left"  json
    right <- readProp "right"  json
    return $ TreeJSON {value: value,left: left, right: right}


instance treeIsForeign :: (IsForeign a) => IsForeign (Tree a) where
  read json = do
    jsonTree :: TreeJSON a <- read json
    fromTreeJSON jsonTree

toJSON :: forall a. Tree a -> String
toJSON tree =
  stringify $ toForeign $ toTreeJSON tree


testJSONString :: String
testJSONString = toJSON (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)))

deserializedTreeJson :: F (TreeJSON Int)
deserializedTreeJson = readJSON testJSONString :: F (TreeJSON Int)


deserializedTree :: F (Tree Int)
deserializedTree = readJSON testJSONString :: F (Tree Int)
