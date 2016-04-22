module Game.GameUtility where

import Game.GameData
import Control.Concurrent
import System.Random

pWhen :: Eq a => a -> a -> (a -> a) -> a
pWhen input test func = if input == test then func input else input

shardsOf :: ServerState -> [ShardState]
shardsOf = shards

clientsOf :: ServerState -> [Client]
clientsOf = clients

removeEmptyShards :: ServerState -> [ShardState] -> [ShardState] 
removeEmptyShards s = filter (not . null . playersShard s)

playersShard :: ServerState -> ShardState -> [Client]
playersShard ss s = filter ((==shardName s) . cliShard) . clientsOf $ ss

shardIsReady :: ServerState -> ShardState -> Bool
shardIsReady ss s = all cliReady $ playersShard ss s

shardIsEmpty :: ServerState -> ShardState -> Bool
shardIsEmpty ss s = null $ playersShard ss s

setClientReady :: Bool -> Client -> Client
setClientReady tf cli = cli {cliReady = tf}

readElem :: (Eq a, Read a) => [a] -> String -> Bool
readElem = (. read) . flip elem 

overShardsOf :: ([ShardState] -> [ShardState]) -> MVar ServerState -> IO ()
overShardsOf func server = modifyMVar_ server $ \s -> return s {shards = func (shardsOf s)} 

overClientsOf :: ([Client] -> [Client]) -> MVar ServerState -> IO ()
overClientsOf func server = modifyMVar_ server $ \s -> return s {clients = func (clientsOf s)}

writeState :: ShardState -> MVar ServerState -> IO ()
writeState gs = overShardsOf $ (gs :) . filter ((/=shardName gs) . shardName) 

randomShardName :: GameDescriptor -> IO String
randomShardName gd = fmap (shardNames gd !!) (randomRIO (0, length (shardNames gd) - 1))
