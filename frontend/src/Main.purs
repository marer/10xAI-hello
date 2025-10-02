module Main where

import Prelude

import Component.UserForm as UserForm
import Component.UserList as UserList
import Data.User (UserResponse)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State =
  { selectedUser :: Maybe UserResponse
  }

data Action
  = Initialize
  | UserCreated UserResponse
  | UserUpdated UserResponse

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { selectedUser: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [ HP.class_ (HH.ClassName "app") ]
  [ HH.h1_ [ HH.text "User Management App" ]
  , HH.div [ HP.class_ (HH.ClassName "container") ]
      [ HH.div [ HP.class_ (HH.ClassName "form-section") ]
          [ HH.slot_ UserForm._component Nothing unit \user -> UserCreated user
          ]
      , HH.div [ HP.class_ (HH.ClassName "list-section") ]
          [ HH.slot_ UserList._component unit unit \_ -> Initialize
          ]
      ]
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  UserCreated user -> do
    H.modify_ _ { selectedUser = Just user }

  UserUpdated user -> do
    H.modify_ _ { selectedUser = Just user }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

