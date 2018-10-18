{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package wschat. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at:

  git://git.devalot.com/wschat.git

No part of this package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Concurrent.STM.TVar
import Control.Concurrent.Supply (Supply)
import qualified Control.Concurrent.Supply as Supply
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Char (isAscii, isAlphaNum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsApp)
import qualified Network.WebSockets as WS
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- | Connected clients (with unique ID so they can be removed too).
type Clients = HashMap Text [(Int, WS.Connection)]

--------------------------------------------------------------------------------
-- | Internal state.
data State = State
  { clients :: TVar Clients
    -- ^ Current clients.

  , supply :: TVar Supply
    -- ^ Fresh supply of unique IDs.

  , codesDir :: FilePath
    -- ^ Directory containing files whose names are checked to see if
    -- there is a matching access code.
  }

--------------------------------------------------------------------------------
-- | Valid messages to forward along to chat clients.
data Message = Message
  { sender :: Text
    -- ^ The user who is sending the message.

  , content :: Text
    -- ^ The content of the message.

  } deriving Generic

instance FromJSON Message
instance ToJSON Message

--------------------------------------------------------------------------------
-- | Error messages that can be sent to clients.
data Error = Error
  { error :: Text
    -- ^ The error message.
  } deriving Generic

instance FromJSON Error
instance ToJSON Error

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  case args of
    [port, dir] -> do
      cs  <- newTVarIO HashMap.empty
      sup <- newTVarIO =<< Supply.newSupply
      Warp.run (read port) (http $ State cs sup dir)

    _ -> die "Usage: wschat <port> <directory>"

--------------------------------------------------------------------------------
-- | Test to see if an access code is valid.
--
-- An access code is valid if a file exists in a configured directory
-- whose name matches the access code.
--
-- For security reasons only alphanumeric ASCII characters are allowed
-- in access codes.
checkAccessCode :: FilePath -> Text -> IO Bool
checkAccessCode dir code = do
    let code' = clean code
    e <- doesFileExist (dir </> code')
    return (not (null code') && e)

  where
    clean :: Text -> FilePath
    clean = Text.unpack . Text.filter isAllowed

    isAllowed :: Char -> Bool
    isAllowed c = isAscii c && isAlphaNum c

--------------------------------------------------------------------------------
-- | HTTP request handler.
http :: State -> Wai.Application
http state request respond = do
    let code = Text.decodeUtf8 (Wai.rawPathInfo request)
    valid <- checkAccessCode (codesDir state) code

    if valid
      then go code
      else respond (Wai.responseLBS HTTP.status401 [] "BAD")

  where
    go :: Text -> IO Wai.ResponseReceived
    go code =
      case websocketsApp WS.defaultConnectionOptions (ws state code) request of
        Nothing  -> respond (Wai.responseLBS HTTP.status400 [] "BAD")
        Just res -> respond res

--------------------------------------------------------------------------------
-- | WebSocket application.
ws :: State         -- ^ Internal state.
   -> Text          -- ^ Access code.
   -> WS.ServerApp  -- ^ WebSocket application.

ws state code pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    bracket (connect conn) disconnect
            (\x -> welcome x >> reader x)

  where

    -- | Post-connection set up.
    connect :: WS.Connection -> IO (Int, WS.Connection)
    connect conn = atomically $ do
      sup <- readTVar (supply state)

      let (n, sup') = Supply.freshId sup
          client    = (n, conn)

      writeTVar (supply state) sup'
      modifyTVar' (clients state) (update code (client:))
      return client

    -- | Post-disconnect tear down.
    disconnect :: (Int, WS.Connection) -> IO ()
    disconnect c = atomically $ do
      let f = \(n, _) -> filter ((/= n) . fst)
      modifyTVar' (clients state) (update code $ f c)

    -- | A welcome message sent to new clients:
    welcome :: (Int, WS.Connection) -> IO ()
    welcome (_, c) = WS.sendTextData c $ Aeson.encode $
                     Message "server" "Welcome to the chat!"

    -- | Read incoming messages from the connection.
    --
    -- Messages are decoded from JSON to ensure they are valid and to
    -- strip away any additional properties that are present.
    reader :: (Int, WS.Connection) -> IO ()
    reader (_, conn) = forever $ do
      msg <- WS.receiveDataMessage conn

      case Aeson.eitherDecode (WS.fromDataMessage msg) of
        Right m -> broadcast m
        Left e  -> let e' = Aeson.encode (Error (Text.pack e))
                   in WS.sendTextData conn e'

    -- | Broadcast message to all clients.
    broadcast :: Message -> IO ()
    broadcast msg = do
      let msg' = Aeson.encode msg
      cs <- readTVarIO (clients state)

      case HashMap.lookup code cs of
        Just cs' -> mapM_ (\(_, c) -> WS.sendTextData c msg') cs'
        Nothing  -> return ()

    -- | Update the client list using a function.
    update :: Text
           -> ([(Int, WS.Connection)] -> [(Int, WS.Connection)])
           -> Clients
           -> Clients
    update key f m = let vs  = fromMaybe [] (HashMap.lookup key m)
                     in HashMap.insert key (f vs) m
