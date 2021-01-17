module ConnectFour.GameThread (
  Event (..),
  Context (..),
  GameThread,
  createContext,
  start,
  kill,
  broadcast,
) where

import ConnectFour.Game (Game, PlayError (..), StartingDisc)
import qualified ConnectFour.Game as Game
import ConnectFour.Game.Board (Column, Disc)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TBQueue, TChan, TVar)
import qualified Control.Concurrent.STM as STM

data Event msg
  = Broadcasted !msg
  | GameUpdated !Game
  | GameOver
  deriving (Show, Eq)

data Move = Move
  { moveDisc :: !Disc
  , moveColumn :: !Column
  }
  deriving (Show, Eq)

data GameThread msg = GameThread
  { gameState :: !(TVar Game)
  , gameInputQueue :: !(TBQueue Move)
  , gameOutputChan :: !(TChan (Event msg))
  , gameThreadId :: !ThreadId
  }

data Context msg = Context
  { contextGetGame :: !(IO Game)
  , contextReceive :: !(IO (Event msg))
  , contextSend :: !(Disc -> Column -> IO ())
  }

createContext :: GameThread msg -> IO (Context msg)
createContext GameThread{..} =
  let get = STM.readTVarIO gameState
      send disc column =
        STM.atomically $ STM.writeTBQueue gameInputQueue (Move disc column)
   in do
        receiveChan <- STM.atomically $ STM.dupTChan gameOutputChan
        let receive = STM.atomically $ STM.readTChan receiveChan
        pure $ Context get receive send

gameLoop :: TVar Game -> TBQueue Move -> TChan (Event msg) -> IO ()
gameLoop state input output = do
  isOnging <- STM.atomically $ do
    game <- STM.readTVar state
    Move disc column <- STM.readTBQueue input
    case Game.play disc column game of
      Left GameIsOver -> pure False
      Left _ -> pure True
      Right updatedGame -> do
        STM.writeTVar state $! updatedGame
        STM.writeTChan output $ GameUpdated updatedGame
        pure . not $ Game.isOver updatedGame
  if isOnging
    then gameLoop state input output
    else STM.atomically $ STM.writeTChan output GameOver

start :: StartingDisc -> IO (GameThread msg)
start startingDisc = do
  state <- STM.newTVarIO $ Game.newGame startingDisc
  inputQueue <- STM.newTBQueueIO 20
  outputChan <- STM.newBroadcastTChanIO
  threadId <- forkIO $ gameLoop state inputQueue outputChan
  pure $
    GameThread
      { gameState = state
      , gameInputQueue = inputQueue
      , gameOutputChan = outputChan
      , gameThreadId = threadId
      }

kill :: GameThread msg -> IO ()
kill GameThread{..} = killThread gameThreadId

broadcast :: msg -> GameThread msg -> IO ()
broadcast msg GameThread{..} =
  STM.atomically $ STM.writeTChan gameOutputChan $ Broadcasted msg
