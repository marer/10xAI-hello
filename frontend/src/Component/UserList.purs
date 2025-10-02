module Component.UserList where

import Prelude

import Api.UserApi (getUsers, deleteUser)
import Data.User (UserResponse, UserId(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)

type State =
  { users :: Array UserResponse
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | LoadUsers
  | UsersLoaded (Either String (Array UserResponse))
  | DeleteUser UserId
  | UserDeleted UserId (Either String Boolean)

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { users: [], loading: false, error: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_
  [ HH.h1_ [ HH.text "Users" ]
  , case state.error of
      Just err -> HH.div [ HP.class_ (HH.ClassName "error") ] [ HH.text err ]
      Nothing -> HH.text ""
  , if state.loading
      then HH.div_ [ HH.text "Loading..." ]
      else HH.div_
          [ HH.button
              [ HE.onClick \_ -> LoadUsers ]
              [ HH.text "Refresh Users" ]
          , HH.ul_ $ map renderUser state.users
          ]
  ]

renderUser :: forall m. UserResponse -> H.ComponentHTML Action () m
renderUser user = HH.li_
  [ HH.text $ user.name <> " (" <> user.email <> ")"
  , HH.button
      [ HE.onClick \_ -> DeleteUser user.id ]
      [ HH.text "Delete" ]
  ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadUsers

  LoadUsers -> do
    H.modify_ _ { loading = true, error = Nothing }
    users <- H.liftAff getUsers
    handleAction $ UsersLoaded users

  UsersLoaded (Left err) -> do
    H.modify_ _ { loading = false, error = Just err }

  UsersLoaded (Right users) -> do
    H.modify_ _ { loading = false, users = users, error = Nothing }

  DeleteUser userId -> do
    result <- H.liftAff $ deleteUser userId
    handleAction $ UserDeleted userId result

  UserDeleted userId (Left err) -> do
    H.modify_ \state -> state { error = Just err }

  UserDeleted userId (Right _) -> do
    H.modify_ \state ->
      state { users = filter (\user -> user.id /= userId) state.users }

