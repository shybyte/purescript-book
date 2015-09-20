module Data.AddressBook.UI where

import Prelude

import DOM

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Traversable (sequence)

import Control.Bind

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Console


valueOf :: forall eff. Field -> Eff (dom :: DOM | eff) String
valueOf field = do
  maybeEl <- querySelector $ getFieldID field
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

appendInlineValidationMessage :: forall eff. ValidationError -> Eff (dom :: DOM | eff) Unit
appendInlineValidationMessage (ValidationError err field)  = do
  maybeEl <- querySelector $ getFieldID field
  case maybeEl of
    Nothing -> return unit
    Just el -> do
      div <- createElement "div"
      setText err div
      addClass "alert" div
      addClass "alert-danger" div
      parentNode <- getParentNode el
      appendChild div parentNode
      return unit


displayValidationErrorsInline :: forall eff. ValidationErrors -> Eff (dom :: DOM | eff) Unit
displayValidationErrorsInline errs = do
  alert <- createElement "div"

  foreachE errs $ \validationError -> do
    appendInlineValidationMessage validationError

  Just validationErrors <- querySelector "#validationErrors"
  alert `appendChild` validationErrors

  return unit

displayValidationErrorsAtTop :: forall eff. ValidationErrors -> Eff (dom :: DOM | eff) Unit
displayValidationErrorsAtTop errs = do
  alert <- createElement "div"

  foreachE errs $ \(ValidationError err field) -> do
    -- div <- createElement "div" >>= setText err
    div <- createElement "div"
    setText err div
    addClass "alert" div
    addClass "alert-danger" div
    div `appendChild` alert
    return unit

  Just validationErrors <- querySelector "#validationErrors"
  alert `appendChild` validationErrors

  return unit


getFieldID :: Field -> String
getFieldID field = "#input" ++ show field



validateControls :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) (Either (Array ValidationError) Person)
validateControls = do
  log "Running validators"

  p <- person <$> valueOf FirstNameField
              <*> valueOf LastNameField
              <*> (address <$> valueOf StreetField
                           <*> valueOf CityField
                           <*> valueOf StateField)
              <*> sequence [ phoneNumber HomePhone <$> valueOf (PhoneField HomePhone)
                           , phoneNumber CellPhone <$> valueOf (PhoneField CellPhone)
                           , phoneNumber WorkPhone <$> valueOf (PhoneField WorkPhone)
                           ]

  return $ validatePerson' p

validateAndUpdateUI :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"
  setInnerHTML "" validationErrors
  remove ".alert"

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> do
      displayValidationErrorsInline errs
      displayValidationErrorsAtTop errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  myBody <- body
  addEventListener "change" validateAndUpdateUI myBody
  return unit
