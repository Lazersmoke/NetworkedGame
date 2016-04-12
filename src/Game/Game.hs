module Game.Game (
  initialize,
  GameDescriptor(..),
  StopCode(..),
  Client(..) 
  ) where
import Control.Monad
import Data.List
import Data.Maybe
import Network.WebSockets as WS
import Control.Concurrent
import Game.GameNetworking
import Game.GameUtility
import Game.GameData

-- Control flow impure code ---------------------------------------------------
initialize :: [GameDescriptor] -> IO ()
initialize gds = do
  state <- newMVar ServerState {
    shards = [],
    clients = [],
    descriptors = gds}
  --We really don't need the thread ids; just let them die with the app
  _ <- forkIO $ mainLoop state
  WS.runServer "0.0.0.0" 9160 $ connHandler state

mainLoop :: MVar ServerState -> IO ()
mainLoop server = do 
  state' <- readMVar server
  removeEmptyShards state' `overShardsOf` server
  state <- readMVar server
  let foundState = find (shardIsReady state) (shardsOf state)
  if isJust foundState
    then do
      let ourDesc = gameDesc . fromJust $ foundState
      void . forkIO $ playGame ourDesc (playersShard state (fromJust foundState)) >>= (void . debugLog . show)
    else threadDelay 100000

