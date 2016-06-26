module Tag where

-- 10.19 Serializing Address Book Entries - 4.Exercise
-- (De)serialize a tagged union.

import Prelude (class Show, ($), show, (<>), return, bind)
import Data.Either (Either(Right, Left))
import Data.JSON (stringify)
import Data.Foreign (F, toForeign)
import Data.Foreign.Class (class IsForeign, readJSON, readProp)

newtype Tagged a b = Tagged (Either a b)

newtype TaggedValue a = TaggedValue {
  tag :: String,
  value :: a
}

toJSON :: forall a b. Tagged a b -> String
toJSON (Tagged (Right right)) =
  stringify $ toForeign $ TaggedValue {
    tag: "right",
    value: right
  }
toJSON (Tagged (Left left)) =
  stringify $ toForeign $ TaggedValue {
    tag: "left",
    value: left
  }


instance taggedValueIsForeign :: (IsForeign a) => IsForeign (TaggedValue a) where
  read json = do
    tag   <- readProp "tag" json
    value <- readProp "value"  json
    return $ TaggedValue {tag: tag,value: value}

instance taggedIsForeign :: (IsForeign a, IsForeign b) => IsForeign (Tagged a b) where
  read json = do
    tag   <- readProp "tag" json
    case tag of
      "left" -> do
        value <- readProp "value" json
        return $ Tagged $ Left value
      otherwise -> do
        value <- readProp "value" json
        return $ Tagged $ Right value

instance taggedShow :: (Show a, Show b) => Show (Tagged a b) where
  show (Tagged either) = show either

instance taggedValueShow :: (Show a) => Show (TaggedValue a) where
  show (TaggedValue taggedValue) = taggedValue.tag <> ":" <> show taggedValue.value

testJSONString :: String
testJSONString = toJSON $ Tagged $ Right "1"

deserializedTaggedValue :: F (TaggedValue String)
deserializedTaggedValue = readJSON testJSONString :: F (TaggedValue String)

deserializedTagged :: F (Tagged String String)
deserializedTagged = readJSON testJSONString :: F (Tagged String String)
