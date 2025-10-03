module Component.UserForm where

import Prelude

import Api.UserApi (createUser, updateUser)
import Data.User (UserId(..), CreateUserRequest, UserResponse)
import Data.Maybe (isJust)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)

type State =
  { name :: String
  , email :: String
  , editing :: Maybe UserId
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | SetEditing (Maybe UserId)
  | SetName String
  | SetEmail String
  | Submit
  | UserCreated (Either String UserResponse)
  | UserUpdated (Either String (Maybe UserResponse))

type Input = Maybe UserId

type Output = UserResponse

component :: forall q m. MonadAff m => H.Component q Input Output m
component = H.mkComponent
  { initialState: \editing ->
      { name: ""
      , email: ""
      , editing: editing
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.form
  [ HE.onSubmit \ev -> Submit ]
  [ HH.h2_ [ HH.text $ if isJust state.editing then "Edit User" else "Create User" ]
  , case state.error of
      Just err -> HH.div [ HP.class_ (HH.ClassName "error") ] [ HH.text err ]
      Nothing -> HH.text ""
  , HH.div_
      [ HH.label_ [ HH.text "Name:" ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.value state.name
          , HE.onValueInput SetName
          , HP.required true
          ]
      ]
  , HH.div_
      [ HH.label_ [ HH.text "Email:" ]
      , HH.input
          [ HP.type_ HP.InputEmail
          , HP.value state.email
          , HE.onValueInput SetEmail
          , HP.required true
          ]
      ]
  , HH.div_
      [ HH.button
          [ HP.type_ HP.ButtonSubmit
          , HP.disabled state.loading
          ]
          [ HH.text $ if state.loading then "Saving..." else "Save" ]
      , HH.button
          [ HP.type_ HP.ButtonButton
          , HE.onClick \_ -> SetEditing Nothing
          ]
          [ HH.text "Cancel" ]
      ]
  ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  SetName name -> do
    H.modify_ _ { name = name }

  SetEmail email -> do
    H.modify_ _ { email = email }

  Submit -> do
    state <- H.get
    if state.name == "" || state.email == ""
      then H.modify_ _ { error = Just "Name and email are required" }
      else do
        H.modify_ _ { loading = true, error = Nothing }
        let req = { name: state.name, email: state.email }
        case state.editing of
          Just userId -> do
            result <- H.liftAff $ updateUser userId req
            handleAction $ UserUpdated result
          Nothing -> do
            result <- H.liftAff $ createUser req
            handleAction $ UserCreated result

  UserCreated (Left err) -> do
    H.modify_ _ { loading = false, error = Just err }

  UserCreated (Right user) -> do
    H.modify_ _ { loading = false, name = "", email = "", error = Nothing }
    H.raise user

  UserUpdated (Left err) -> do
    H.modify_ _ { loading = false, error = Just err }

  UserUpdated (Right (Just user)) -> do
    H.modify_ _ { loading = false, name = "", email = "", editing = Nothing, error = Nothing }
    H.raise user

  UserUpdated (Right Nothing) -> do
    H.modify_ _ { loading = false, error = Just "User not found" }

  SetEditing editing -> do
    H.modify_ _ { editing = editing, name = "", email = "", error = Nothing }

