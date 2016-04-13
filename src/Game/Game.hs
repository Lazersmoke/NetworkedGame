module Game.Game (
  initialize,
  GameDescriptor(..),
  StopCode(..),
  Client(..),
  tellClient,
  tellClients,
  askClient
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
  when (any (shardIsEmpty state') (shards state')) $ putStrLn "Garbage"
  filter (not . shardIsEmpty state') `overShardsOf` server
  state <- readMVar server
  let foundState = find (shardIsReady state) (shardsOf state)
  if isJust foundState
    then do
      map (\x -> if cliShard x == shardName (fromJust foundState)
        then x {cliReady = False} 
        else x) `overClientsOf` server
      let ourDesc = gameDesc . fromJust $ foundState
      --doesnt unready TODO
      _ <- forkIO $ do 
        playGame ourDesc (playersShard state (fromJust foundState)) >>= debugLog . show
        return ()
      return ()
    else threadDelay 100000
  mainLoop server
