{-# LANGUAGE OverloadedStrings #-}
module Game.GameNetworking where

import Game.GameData
import Data.List
import Data.Maybe
import Network.WebSockets as WS
import qualified Data.Text as T
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Applicative
import qualified Control.Monad.Parallel as Parr
import Game.GameUtility
connHandler :: MVar ServerState -> WS.ServerApp
connHandler state connection = do
  debugLog "Got connection"
  -- Accept Connection
  accepted <- WS.acceptRequest connection
  -- Fork ping thread to keep connection alive
  WS.forkPingThread accepted 30
  -- Wait for username message
  msg <- WS.receiveData accepted
  -- Acknowledge connection
  WS.sendTextData accepted $ T.pack ("Welcome, " ++ T.unpack (T.drop 9 msg))
  debugLogClient (T.unpack $ T.drop 9 msg) (T.unpack msg)
  --Send list of possible shards, refreshing it every second
  shardingThread <- forkIO . forever $ do
    shardList <- shardListing state
    WS.sendTextData accepted $ T.pack shardList
    threadDelay 1000000

  shardChoice <- WS.receiveData accepted
  killThread shardingThread
  st <- readMVar state
  debugLogClient (T.unpack $ T.drop 9 msg) $ T.unpack shardChoice
  case msg of
      -- Wrong Prefix
    _ | not $ prefix `T.isPrefixOf` msg -> do
          debugLogClient (cliName client) "Sending Bad" 
          tellClient "Wrong message prefix, disconnecting" client
          disconnect
      -- Bad characters in username or username excplicity disallowed
      | any (`elem` cliName client) bannedChars || elem (cliName client) (bannedNames ++ map cliName (clientsOf st)) -> do
          debugLog $ "Client attempted to join with bad username: " ++ cliName client
          tellClient "Info|Bad username" client
          disconnect
      -- Make new shard
      | "New|" `T.isPrefixOf` shardChoice -> flip finally disconnect $ do
          -- Generate a fresh shard for this player, with only them in it
          let maybeGD = find ((==drop 4 shard) . descName) (descriptors st)
          if isJust maybeGD
            then do
              let ourGD = fromJust maybeGD
              newshard <- createShard ourGD <$> randomShardName ourGD
              debugLogClient (cliName client) $ "Spawning new shard " ++ shardName newshard 
              (newshard:) `overShardsOf` state
              (client:) `overClientsOf` state
              --Don't need to broadcast join into new shard
              tellClient ("Join|" ++ cliName client) client
              tellClient ("Connected to: " ++ shardName newshard) client
              waitForGame (cliConn client) ourGD client
            else tellClient "Invalid Game Name" client >> disconnect
      -- Invalid shard
      | shard `notElem` map shardName (shardsOf st) -> do
          debugLogClient (cliName client) "Sending invalid shard" 
          tellClient "Invalid shard, disconnecting" client
          disconnect
      -- All is peachy
      | otherwise -> flip finally disconnect $ do
          debugLogClient (cliName client) $ "Connecting to shard " ++ shard
          tellClient ("Connected to: " ++ shard) client
          st' <- readMVar state
          -- On the clock now
          let theshard = fromJust $ find ((==shard) . shardName) (shardsOf st')
          (onPlayerJoin . gameDesc $ theshard) client
          -- Broadcast our connection to everyone else
          mapM_ (flip WS.sendTextData (T.pack $ "Join|" ++ cliName client) . cliConn) $ playersShard st' theshard
          -- Retrocast everyone elses connection to our connection
          mapM_ (WS.sendTextData accepted . T.pack . ("Join|"++) . cliName) . reverse $ playersShard st' theshard
          -- Add client
          (client:) `overClientsOf` state
          waitForGame (cliConn client) (gameDesc theshard) client
 
      where
        bannedNames = ["Info","Join","Ask"," ","","|",","]
        bannedChars = ['|',',','\"','\'']
        shard = T.unpack shardChoice
        prefix = T.pack "Hi! I am "
        client = Client {
          cliName = T.unpack $ T.drop (T.length prefix) msg,
          cliConn = accepted,
          cliShard = shard,
          cliReady = False}
        disconnect = do
          -- Remove client and return new state
          debugLog $ "Disconnecting client: " ++ cliName client
          nst <- readMVar state
          --Tell everyone about disconnect
          mapM_ (tellClient $ "Disconnect|" ++ cliName client) (clientsOf nst)
          --Remove the client
          filter ((/= cliName client) . cliName) `overClientsOf` state

waitForGame :: WS.Connection -> GameDescriptor -> Client -> IO ()
waitForGame connection gd client = forever $ do
  --Block until we get a client message, then unpack it
  msg <- T.unpack <$> WS.receiveData connection
  --Log to console if we need to (debug)
  debugLogClient (cliName client) msg
  --Call GD's function
  onMessage gd client msg
  --NOTE: This works because
  --  onMessage :: GameDescriptor -> Client -> String -> IO ()
  --due to currying

shardListing :: MVar ServerState -> IO String
shardListing ss = do
  st <- readMVar ss
  return $ "Shards: " ++ unwords (map (\x -> "\n" ++ shardName x ++ ": " ++ show (length $ playersShard st x)) (shardsOf st))

tellEveryone :: String -> ServerState -> IO ()
tellEveryone s ss = tellClients s (clientsOf ss)

tellClients :: String -> [Client] -> IO ()
tellClients s = Parr.mapM_ (tellClient s) 

tellClient :: String -> Client -> IO ()
tellClient s cl = WS.sendTextData (cliConn cl) . T.pack $ s

askClient :: String -> Client -> IO ()
askClient = tellClient . ("Ask|" ++) 