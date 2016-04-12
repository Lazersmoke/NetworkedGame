module Game.GameData where

import Network.WebSockets as WS
import Control.Monad

data StopCode = Stop String | Continue deriving (Show,Eq,Read)
data ServerState = ServerState {
  shards :: [ShardState],
  clients :: [Client],
  descriptors :: [GameDescriptor]}
data Client = Client {
  cliName :: String,
  cliReady :: Bool,
  cliConn :: WS.Connection,
  cliShard :: String}
  
instance Eq Client where
  a == b = cliName a == cliName b

data GameDescriptor = GameDescriptor {
  playGame :: [Client] -> IO StopCode,
  descName :: String,
  shardNames :: [String],
  onMessage :: Client -> String -> IO ()}

data ShardState = ShardState {
  gameDesc :: GameDescriptor,
  shardName :: String}

-- Build a shard with a Descriptor and a name
createShard :: GameDescriptor -> String -> ShardState
createShard gd s = ShardState {gameDesc = gd, shardName = s}

debug :: Bool
debug = True

debugLog :: String -> IO ()
debugLog s = when debug $ putStrLn s

debugLogClient :: String -> String -> IO ()
debugLogClient clientName s = when debug $ putStrLn $ "<" ++ clientName ++ "> " ++ s

consoleLog :: String -> IO ()
consoleLog = putStrLn . ("[Server]"++)
