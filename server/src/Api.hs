{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Api where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Auth.Server (Auth, AuthResult, FromJWT, JWT
                          , SetCookie, ToJWT)
import qualified Elm.Derive

type PublicApi = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] String

type Api =
  "api" :>
    ("item" :> Get '[JSON] [ItemId] :<|>
     "item" :> Capture "itemId" ItemId :> Get '[JSON] Item :<|>
     "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
     "item" :> Capture "itemId" ItemId :> Delete '[JSON] ())

type PrivateApi =
  Auth '[JWT] User :> Api

type ClientApi =
  PublicApi :<|> Api

api :: Proxy Api
api = Proxy

-- types

newtype ItemId = ItemId Int
  deriving (Show, Eq, Ord, Enum, FromHttpApiData, ToHttpApiData)

data LoginForm
  = LoginForm {
    userName :: String
  , loginPassword :: String
  }
  deriving (Show, Eq)

data User
  = User {
  userId :: Int,
  name :: String,
  password :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User
instance ToJWT User
instance FromJWT User


data Item
  = Item {
    id :: ItemId,
    text :: String
  }
  deriving (Show, Eq)

Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''Item
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''ItemId
Elm.Derive.deriveBoth Elm.Derive.defaultOptions ''LoginForm
