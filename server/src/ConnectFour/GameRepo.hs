module ConnectFour.GameRepo (
  PlayerMsg (..),
  GameId,
  GameRepo,
  newGameRepo,
  PendingGames (..),
  listGames,
  OnlineGameOptions (..),
  GameContext,
  YourDiscColor,
  createGame,
  WithMaybeGameContext,
  joinGame,
) where

import ConnectFour.Game (StartingDisc)
import ConnectFour.Game.Board (Disc)
import ConnectFour.Game.ColumnStack (oppositeDisc)
import ConnectFour.GameThread (Context (..), GameThread)
import qualified ConnectFour.GameThread as GameThread
import ConnectFour.PlayerName (PlayerName)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (bracket)
import Data.Aeson (FromJSON, KeyValue (..), ToJSON (..), object, pairs)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

data PlayerMsg
  = PlayerJoined !PlayerName
  | PlayerLeft !PlayerName
  deriving (Show, Eq)

data OnlineGame = OnlineGame
  { ogThread :: !(GameThread PlayerMsg)
  , ogPlayer1Name :: !PlayerName
  , ogPlayer1Disc :: !Disc
  }

newtype GameId = GameId UUID
  deriving (Show, Eq, Ord, ToJSON, FromJSON) via UUID

newGameId :: IO GameId
newGameId = GameId <$> UUID.nextRandom

newtype GameRepo = GameRepo (TVar (Map GameId OnlineGame))

newGameRepo :: IO GameRepo
newGameRepo = GameRepo <$> STM.newTVarIO Map.empty

data PendingGames = PendingGames
  { pgGameId :: !GameId
  , pgPlayer1Name :: !PlayerName
  , pgPlayer1Disc :: !Disc
  }
  deriving (Show, Eq)

instance ToJSON PendingGames where
  toJSON PendingGames{..} =
    object
      [ "gameId" .= pgGameId
      , "player1Name" .= pgPlayer1Name
      , "player1Disc" .= pgPlayer1Disc
      ]
  toEncoding PendingGames{..} =
    pairs . mconcat $
      [ "gameId" .= pgGameId
      , "player1Name" .= pgPlayer1Name
      , "player1Disc" .= pgPlayer1Disc
      ]

listGames :: GameRepo -> IO [PendingGames]
listGames (GameRepo onlineGames) = do
  games <- Map.assocs <$> STM.readTVarIO onlineGames
  pure $
    games <&> \(gameId, OnlineGame{..}) ->
      PendingGames
        { pgGameId = gameId
        , pgPlayer1Name = ogPlayer1Name
        , pgPlayer1Disc = ogPlayer1Disc
        }

data OnlineGameOptions = OnlineGameOptions
  { ogoPlayer1Name :: !PlayerName
  , ogoPlayer1Disc :: !Disc
  , ogoStartingDisc :: !StartingDisc
  }
  deriving (Show, Eq)

type GameContext = Context PlayerMsg

type YourDiscColor = Disc

type WithGameContext = ((GameId, YourDiscColor, GameContext) -> IO ()) -> IO ()

createGame :: OnlineGameOptions -> GameRepo -> WithGameContext
createGame OnlineGameOptions{..} (GameRepo onlineGames) action = do
  let create = do
        gameId <- newGameId
        thread <- GameThread.start ogoStartingDisc
        let onlineGame = OnlineGame thread ogoPlayer1Name ogoPlayer1Disc
        STM.atomically $ STM.modifyTVar' onlineGames (Map.insert gameId onlineGame)
        context <- GameThread.createContext thread
        pure (thread, (gameId, ogoPlayer1Disc, context))
      destroy (thread, (gameId, _, _)) = do
        STM.atomically $ STM.modifyTVar' onlineGames (Map.delete gameId)
        GameThread.broadcast (PlayerLeft ogoPlayer1Name) thread
        GameThread.kill thread
   in bracket create destroy (action . snd)

type OtherPlayerName = PlayerName

type WithMaybeGameContext = (Maybe (OtherPlayerName, YourDiscColor, GameContext) -> IO ()) -> IO ()

joinGame :: GameId -> PlayerName -> GameRepo -> WithMaybeGameContext
joinGame gameId player2Name (GameRepo onlineGames) action =
  let create = do
        mbOnlineGame <- STM.atomically $ do
          mbOnlineGame' <- Map.lookup gameId <$> STM.readTVar onlineGames
          case mbOnlineGame' of
            Nothing -> pure Nothing
            Just onlineGame -> do
              STM.modifyTVar' onlineGames (Map.delete gameId)
              pure $ Just onlineGame
        case mbOnlineGame of
          Nothing -> pure Nothing
          Just OnlineGame{..} -> do
            GameThread.broadcast (PlayerJoined player2Name) ogThread
            context <- GameThread.createContext ogThread
            pure $ Just (ogThread, (ogPlayer1Name, oppositeDisc ogPlayer1Disc, context))
      destroy resource =
        case resource of
          Just (thread, _) -> do
            GameThread.broadcast (PlayerLeft player2Name) thread
            GameThread.kill thread
          Nothing -> pure ()
   in bracket create destroy $ action . fmap snd
