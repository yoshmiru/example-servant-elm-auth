{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators, FlexibleInstances  #-}

module App where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack)
import Data.Map
import Network.Wai
import Network.Wai.MakeAssets
import Servant
import Servant.Auth.Server as SAS

import Api

type PublicWithAssets = PublicApi :<|> Raw
type WithAssets = PrivateApi :<|> PublicWithAssets

withAssets :: Proxy WithAssets
withAssets = Proxy

options :: Options
options = Options "client"

app :: IO Application
app = do
  db     <- mkDB
  assets <- serveAssets options
  myKey <- SAS.generateKey
  let jwtCfg = defaultJWTSettings myKey
      cookieCfg = defaultCookieSettings
      cfg = jwtCfg :. cookieCfg :. EmptyContext
      server = privateServer db :<|> publicServer cookieCfg jwtCfg db assets
  pure  $ serveWithContext withAssets cfg server

publicServer :: CookieSettings -> JWTSettings -> DB -> Application -> Server PublicWithAssets
publicServer cs jwts db assets = (publicApiServer cs jwts db :<|> Tagged assets)

privateServer :: DB -> Server PrivateApi
privateServer db (Authenticated user) = privateApiServer db user
privateServer db _ = throwAll err401

publicApiServer :: CookieSettings -> JWTSettings -> DB -> Server PublicApi
publicApiServer cs jwts db = login cs jwts db

privateApiServer :: DB -> User -> Server Api
privateApiServer db _ = listItems db :<|> getItem db :<|> postItem db :<|> deleteItem db

login :: CookieSettings -> JWTSettings -> DB -> LoginForm
                        -> Handler String
login cs jwts db (LoginForm userName password) = do
  let toUser userId = User userId userName password
      user = case (userName , password) of
        ("user1", "password") -> toUser 1
        ("user2", "password") -> toUser 2
  mCookie <- liftIO $ makeSessionCookieBS cs jwts user
  case mCookie of
    Just cookie -> return $ unpack cookie
    _ -> throwAll err401

listItems :: DB -> Handler [ItemId]
listItems db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwError err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new = liftIO $ insertItem db new

-- fake DB

newtype DB = DB (MVar (Map ItemId String))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print

mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> String -> IO ItemId
insertItem (DB mvar) new = modifyMVar mvar $ \m -> do
  let newKey = case keys m of
        [] -> ItemId 0
        ks -> succ (maximum ks)
  return (insert newKey new m, newKey)

lookupItem :: DB -> ItemId -> IO (Maybe Item)
lookupItem (DB mvar) i = fmap (Item i) . Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [ItemId]
allItemIds (DB mvar) = keys <$> readMVar mvar

deleteItem :: MonadIO m => DB -> ItemId -> m ()
deleteItem (DB mvar) i = liftIO $ do
  modifyMVar_ mvar $ \m -> return (delete i m)
  return ()

