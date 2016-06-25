module Main where

import Prelude
import Data.Function
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Null
import Data.Foreign.Class
import Data.JSON
import Data.Traversable
import Data.AddressBook
import Data.AddressBook.UI
import DOM
import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Alert
import Control.Monad.Eff.Storage
import Control.Monad.Eff.Console
import Data.Array (concatMap, length, range)
import Data.URI (encodeURIComponent, unsafeHead)
import Math ((%))

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String

  , street     :: String
  , city       :: String
  , state      :: String

  , homePhone  :: String
  , cellPhone  :: String
  }


-- class IsForeign a where
--   read :: Foreign -> F a

instance formDataIsForeign :: IsForeign FormData where
  read value = do
    firstName   <- readProp "firstName" value
    lastName    <- readProp "lastName"  value

    street      <- readProp "street"    value
    city        <- readProp "city"      value
    state       <- readProp "state"     value

    homePhone   <- readProp "homePhone" value
    cellPhone   <- readProp "cellPhone" value

    return $ FormData
      { firstName  : firstName
      , lastName   : lastName

      , street     : street
      , city       : city
      , state      : state

      , homePhone  : homePhone
      , cellPhone  : cellPhone
      }


-- 10.19 Serializing Address Book Entries - 2.Exercise
-- Rewrite the formDataIsForeign type class instance
-- to use the applicative combinators <$> and <*> .

createFormData :: String -> String -> String -> String -> String -> String -> String -> FormData
createFormData firstName lastName street city state homePhone cellPhone = FormData {
    firstName:  firstName
  , lastName:  lastName
  , street     : street
  , city       : city
  , state      : state

  , homePhone  : homePhone
  , cellPhone  : cellPhone
}

-- class IsForeign a where
--   read :: Foreign -> F a
instance formData2IsForeign :: IsForeign FormData where
  read value =
    createFormData <$> readProp "firstName" value
    <*> readProp "lastName" value
    <*> readProp "street" value
    <*> readProp "city" value
    <*> readProp "state" value
    <*> readProp "homePhone" value
    <*> readProp "cellPhone" value

toFormData :: Person -> FormData
toFormData (Person p@{ address = Address a
                     , phones = [ PhoneNumber pn1
                                , PhoneNumber pn2
                                ]
                     }) =
  FormData { firstName  : p.firstName
           , lastName   : p.lastName

           , street     : a.street
           , city       : a.city
           , state      : a.state

           , homePhone  : pn1.number
           , cellPhone  : pn2.number
           }


updateForm :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit
updateForm sel value = do
  Just element <- querySelector sel
  setValue value element
  return unit

loadSavedData :: forall eff. Eff (console :: CONSOLE, alert :: ALERT, dom :: DOM, storage :: STORAGE | eff) Unit
loadSavedData = do
  item :: Foreign <- getItem "person"

  let
    savedData :: F (Maybe FormData)
    savedData = do
      let readItem = (read item) :: F (Null String)
      jsonOrNull :: Null String <- readItem
      let x = runNull jsonOrNull :: Maybe String
      -- traverse :: forall a b f. (Applicative f) => (a -> f b) -> t a -> f (t b)
      -- traverse :: forall a b.(String -> F FormData) -> Maybe String -> F (Maybe FormData)
      traverse readJSON x

  case savedData of
    Left err -> alert $ "Unable to read saved form data: " ++ show err
    Right Nothing -> return unit
    Right (Just (FormData o)) -> do
      updateForm "#inputFirstName" o.firstName
      updateForm "#inputLastName"  o.lastName

      updateForm "#inputStreet"    o.street
      updateForm "#inputCity"      o.city
      updateForm "#inputState"     o.state

      updateForm "#inputHomePhone" o.homePhone
      updateForm "#inputCellPhone" o.cellPhone

      return unit

validateAndSaveEntry :: forall eff. Eff (console :: CONSOLE, alert :: ALERT, dom :: DOM, storage :: STORAGE | eff) Unit
validateAndSaveEntry = do
  log "Running validators"

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> alert $ "There are " ++ show (length errs) ++ " validation errors."
    Right result -> do
      setItem "person" $ stringify $ toForeign $ toFormData result
      alert "Saved"

  return unit

main :: forall eff. Eff (console :: CONSOLE, alert :: ALERT, dom :: DOM, storage :: STORAGE | eff) Unit
main = do
  log "Loading data from local storage"
  log (encodeURIComponent "123&=")
  -- let h = unsafeHead []
  let d = runFn2 divides 2.0 10.0
  log $ "runFn2 divides 2.0 10.0 = " <> show d
  loadSavedData

  log "Attaching event handlers"
  setupEventHandlers

  Just saveButton <- querySelector "#saveButton"

  addEventListener "click" validateAndSaveEntry saveButton

  return unit


myRange = range

-- 10.13 Functions of Multiple Arguments
divides :: Fn2 Number Number Boolean
divides = mkFn2 $ \n m -> m % n == 0.0

confirmTest = do
  answer <- confirm "Hossa"
  log $ "Answer: " <> show answer


localStorageTest = do
  setItem "key" "value"
  getItem "key"

localStorageTest2 = do
  removeItem "key"


-- 10.19 Serializing Address Book Entries -  1.Exercise
-- Use readJSON to parse a JSON document representing a two-dimensional JavaScript array of integers,
-- such as [[1, 2, 3], [4, 5], [6]] .

testReadJson = do
  log $ show $  readJSON "[[1, 2, 3], [4, 5], [6]]" :: F (Array (Array Int))
  -- What if the elements are allowed be null?
  log $ show $ map (\el -> map runNull el) <$> readJSON "[[1, 2, 3], [4, null], [6]]" :: F (Array (Array (Null Int)))
  -- What if the arrays themselves are allowed to be null?
  log $ show $ map runNull <$> readJSON "[[1, 2, 3], [4], null]" :: F (Array (Null (Array Int)))
