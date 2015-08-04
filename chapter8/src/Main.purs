module Main where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either
import Data.Foreign
import Data.Foreign.Index (prop)
import Data.Foldable (for_)

import Data.AddressBook
import Data.AddressBook.Validation

import Control.Monad.Eff
import Control.Monad.Eff.Console

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }
  
initialState :: AppState
initialState = AppState 
  { person: examplePerson
  , errors: []
  }

valueOf :: forall eff. Event -> F String
valueOf e = do
  target <- prop "target" (toForeign e)
  value <- prop "value" target
  readString value
          
updateAppState :: forall eff. UIRef -> 
                              (String -> Person) -> 
                              Event -> 
                              Eff (console :: CONSOLE, state :: ReactState (Read Write) AppState | eff) Unit
updateAppState ctx update e = do
  val <- readState ctx
  
  for_ (valueOf e) \s -> do
    let newPerson = update s
    
    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> writeState ctx (AppState { person: newPerson, errors: errors })
      Right _ -> writeState ctx (AppState { person: newPerson, errors: [] })

addressBook :: forall props. props -> UI
addressBook = mkUI $ spec initialState \ctx -> do
  AppState { person: person, errors: errors } <- readState ctx
  
  let renderValidationError err = D.li' [ D.text err ]
  
      renderValidationErrors [] = []
      renderValidationErrors errors = 
        [ D.div [ P.className "alert alert-danger" ] 
                [ D.ul' (map renderValidationError errors) ] 
        ]
  
      formField name hint value update =
        D.div [ P.className "form-group" ]
              [ D.label [ P.className "col-sm-2 control-label" ]
                        [ D.text name ]
              , D.div [ P.className "col-sm-3" ]
                      [ D.input [ P._type "text"
                                , P.className "form-control"
                                , P.placeholder hint
                                , P.value value
                                , P.onChange (updateAppState ctx update)
                                ] []
                      ]
              ]
              
      renderPhoneNumber (PhoneNumber phone) index =
        formField (show phone."type") "XXX-XXX-XXXX" phone.number \s -> 
          Person $ person' { phones = fromMaybe person'.phones $ modifyAt index (updatePhoneNumber s) person'.phones }
  
      person' = runPerson person
      address' = runAddress person'.address
              
      updateFirstName s = Person $ person' { firstName = s }
      updateLastName  s = Person $ person' { lastName  = s }
  
      updateStreet s = Person $ person' { address = Address $ address' { street = s } }
      updateCity   s = Person $ person' { address = Address $ address' { city   = s } }
      updateState  s = Person $ person' { address = Address $ address' { state  = s } }
      
      updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }
  
  return $ 
    D.div [ P.className "container" ] 
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , D.div [ P.className "row" ] 
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Basic Information" ] 
                           
                           , formField "First Name" "First Name" person'.firstName updateFirstName
                           , formField "Last Name"  "Last Name"  person'.lastName  updateLastName
                           
                           , D.h3' [ D.text "Address" ] 
                           
                           , formField "Street" "Street" address'.street updateStreet
                           , formField "City"   "City"   address'.city   updateCity
                           , formField "State"  "State"  address'.state  updateState
                           
                           , D.h3' [ D.text "Contact Information" ] 
                           ]
                           ++ zipWith renderPhoneNumber person'.phones (0 .. length person'.phones)
                  ]
          ]

main = do
  log "Rendering address book component"
  let component = D.div [] [ addressBook {} ]
  renderToBody component