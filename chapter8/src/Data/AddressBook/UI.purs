module Data.AddressBook.UI where

import Control.Monad.Eff.Console
import Data.AddressBook.Validation
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.DOM (addEventListener, body, setInnerHTML, querySelector, appendChild, addClass, setText, createElement, getValue)
import DOM (DOM)
import Data.AddressBook (Person, PhoneType(CellPhone, HomePhone, WorkPhone), phoneNumber, address, person)
import Data.Either (Either(Right, Left))
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (sequence)
import Prelude (Unit, unit, return, bind, ($), (<$>), (<*>), (++))

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""


getErrorText :: ValidationError -> String
getErrorText (ValidationError t _) = t


getErrorField :: ValidationError -> Field
getErrorField (ValidationError _ f) = f

allFields :: Array Field
allFields =
  [FirstNameField, LastNameField,
  StreetField, CityField, StateField,
  PhoneFields, PhoneField WorkPhone, PhoneField HomePhone, PhoneField CellPhone]

displayValidationErrors :: forall eff. Errors -> Eff (dom :: DOM | eff) Unit
displayValidationErrors errs = do
  -- Remove old Validation Messages
  foreachE allFields $ \field -> do
    Just validationErrors <- querySelector $ toValidationErrorsSelector field
    setInnerHTML "" validationErrors
    return unit
  -- Add new Validation Messages
  foreachE errs $ \err -> do
    let field = getErrorField err
    Just validationErrors <- querySelector $ toValidationErrorsSelector field
    div <- createElement "div"
    setText (getErrorText err) div
    addClass "alert" div
    addClass "alert-danger" div
--  div <- createElement "div"
--    >>= setText err
--    >>= addClass "alert"
--    >>= addClass "alert-danger"
    div `appendChild` validationErrors
    return unit
  return unit

toValidationErrorsSelector :: Field -> String
toValidationErrorsSelector field = (toSelector field) ++ " + .validationErrors"

validateControls :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) (Either (Array ValidationError) Person)
validateControls = do
  log "Running validators"

  p <- person <$> valueOf (toSelector FirstNameField)
              <*> valueOf (toSelector LastNameField)
              <*> (address <$> valueOf (toSelector StreetField)
                           <*> valueOf (toSelector CityField)
                           <*> valueOf (toSelector StateField))
              <*> sequence [ phoneNumber HomePhone <$> valueOf (toSelector (PhoneField HomePhone))
                           , phoneNumber CellPhone <$> valueOf (toSelector (PhoneField CellPhone))
                           , phoneNumber WorkPhone <$> valueOf (toSelector (PhoneField WorkPhone))
                           ]

  return $ validatePerson' p

toSelector :: Field -> String
toSelector field =
  case field of
    FirstNameField -> "#inputFirstName"
    LastNameField -> "#inputLastName"
    StreetField -> "#inputStreet"
    CityField -> "#inputCity"
    StateField -> "#inputState"
    PhoneFields -> "#phones"
    (PhoneField HomePhone) -> "#inputHomePhone"
    (PhoneField CellPhone) -> "#inputCellPhone"
    (PhoneField WorkPhone) -> "#inputWorkPhone"
    (PhoneField _) -> ""


validateAndUpdateUI :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"
  setInnerHTML "" validationErrors

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> displayValidationErrors errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  bodyEl <- body
  addEventListener "change" validateAndUpdateUI bodyEl
  -- body >>= addEventListener "change" validateAndUpdateUI

  return unit
