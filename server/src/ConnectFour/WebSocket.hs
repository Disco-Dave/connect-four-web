module ConnectFour.WebSocket (serverApp) where

import ConnectFour.Game (Game)
import qualified ConnectFour.Game as Game
import ConnectFour.Game.Board (Column (..))
import ConnectFour.GameRepo (GameId, GameRepo, OnlineGameOptions (..), PlayerMsg)
import qualified ConnectFour.GameRepo as GameRepo
import ConnectFour.GameThread (Context (..), Event)
import qualified ConnectFour.GameThread as GameThread
import ConnectFour.PlayerName (PlayerName)
import qualified Control.Concurrent.Async as Async
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseField)
import qualified Data.Char as Char
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.WebSockets as WebSockets
import System.Timeout (timeout)

data InitialMsg
  = NewGame !OnlineGameOptions
  | JoinGame !GameId !PlayerName
  deriving (Show, Eq)

instance FromJSON InitialMsg where
  parseJSON = Aeson.withObject "InitialMessage" $ \obj -> do
    type_ <- Text.map Char.toLower . Text.strip <$> Aeson.parseField @Text obj "_type"
    case type_ of
      "new" ->
        let opts =
              OnlineGameOptions
                <$> (obj .: "player1Name")
                <*> (obj .: "player1Disc")
                <*> (obj .: "startingDisc")
         in fmap NewGame opts
      "join" -> JoinGame <$> (obj .: "gameId") <*> (obj .: "player2Name")
      _ -> fail "Unrecognized \"_type\". May only be \"new\" or \"join\"."

newtype MoveMsg = MoveMsg Column

instance FromJSON MoveMsg where
  parseJSON value = do
    number <- parseJSON @Int value
    MoveMsg <$> case number of
      1 -> pure Column1
      2 -> pure Column2
      3 -> pure Column3
      4 -> pure Column4
      5 -> pure Column5
      6 -> pure Column6
      7 -> pure Column7
      _ -> fail "Only columns 1 through 7 are valid."

data OutgoingMsg
  = PlayerJoined !PlayerName
  | PlayerLeft !PlayerName
  | GameUpdated !Game
  | GameOver
  deriving (Show, Eq)

outgoingMsgToKeyValues :: KeyValue kv => OutgoingMsg -> [kv]
outgoingMsgToKeyValues msg = case msg of
  PlayerJoined playerName ->
    [ "_type" .= ("playerJoined" :: Text)
    , "playerName" .= playerName
    ]
  PlayerLeft playerName ->
    [ "_type" .= ("playerLeft" :: Text)
    , "playerName" .= playerName
    ]
  GameOver ->
    [ "_type" .= ("gameOver" :: Text)
    ]
  GameUpdated game ->
    [ "_type" .= ("gameUpdated" :: Text)
    , "status" .= Game.status game
    , "board" .= Game.board game
    ]

instance ToJSON OutgoingMsg where
  toJSON = Aeson.object . outgoingMsgToKeyValues
  toEncoding = Aeson.pairs . mconcat . outgoingMsgToKeyValues

getInitialMsg :: WebSockets.Connection -> IO (Maybe InitialMsg)
getInitialMsg conn = do
  msg <- timeout 300_000_000 (WebSockets.receiveData conn)
  pure $ msg >>= Aeson.decode

handleUserInput :: (Column -> IO ()) -> WebSockets.Connection -> IO ()
handleUserInput send conn = do
  mbMove <- Aeson.decode @MoveMsg <$> WebSockets.receiveData conn
  case mbMove of
    Nothing -> pure ()
    Just (MoveMsg column) -> send column
  handleUserInput send conn

handleEvents :: IO (Event PlayerMsg) -> WebSockets.Connection -> IO ()
handleEvents receive conn = do
  event <-
    receive <&> \case
      GameThread.Broadcasted (GameRepo.PlayerJoined playerName) ->
        PlayerJoined playerName
      GameThread.Broadcasted (GameRepo.PlayerLeft playerName) ->
        PlayerLeft playerName
      GameThread.GameOver -> GameOver
      GameThread.GameUpdated game -> GameUpdated game
  WebSockets.sendTextData conn (Aeson.encode event)
  case event of
    GameOver -> pure ()
    _ -> handleEvents receive conn

startUserThreads :: IO (Event PlayerMsg) -> (Column -> IO ()) -> WebSockets.Connection -> IO ()
startUserThreads receive send conn = do
  _ <- Async.race (handleEvents receive conn) (handleUserInput send conn)
  pure ()

handleNewGame :: OnlineGameOptions -> GameRepo -> WebSockets.Connection -> IO ()
handleNewGame opts repo conn =
  GameRepo.createGame opts repo $ \(gameId, disc, ctx) -> do
    let Context getGame receive send = ctx
    game <- getGame
    let msg =
          Aeson.object
            [ "_type" .= ("newReceived" :: Text)
            , "gameId" .= gameId
            , "yourDiscColor" .= disc
            , "board" .= Game.board game
            , "status" .= Game.status game
            ]
    WebSockets.sendTextData conn (Aeson.encode msg)
    startUserThreads receive (send disc) conn

handleJoinGame :: GameId -> PlayerName -> GameRepo -> WebSockets.Connection -> IO ()
handleJoinGame gameId playerName repo conn =
  GameRepo.joinGame gameId playerName repo $ \case
    Nothing ->
      WebSockets.sendTextData @Text conn "\"Game not found.\""
    Just (otherPlayerName, disc, ctx) -> do
      let Context getGame receive send = ctx
      game <- getGame
      let msg =
            Aeson.object
              [ "_type" .= ("joinReceived" :: Text)
              , "gameId" .= gameId
              , "yourDiscColor" .= disc
              , "board" .= Game.board game
              , "status" .= Game.status game
              , "otherPlayerName" .= otherPlayerName
              ]
      WebSockets.sendTextData conn (Aeson.encode msg)
      startUserThreads receive (send disc) conn

serverApp :: GameRepo -> WebSockets.ServerApp
serverApp repo pending = do
  conn <- WebSockets.acceptRequest pending
  WebSockets.withPingThread conn 30 (pure ()) $ do
    msg <- getInitialMsg conn
    case msg of
      Nothing -> pure ()
      Just (NewGame opts) ->
        handleNewGame opts repo conn
      Just (JoinGame gameId playerName) ->
        handleJoinGame gameId playerName repo conn
