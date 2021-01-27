module ConnectFour.IntegrationSpec where

import qualified ConnectFour
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import Control.Exception (bracket, onException)
import Control.Monad (forever)
import Data.Aeson (KeyValue ((.=)), object, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text, strip)
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets
import Test.Hspec (Spec, around, it, runIO, shouldBe, shouldContain, shouldMatchList, shouldNotSatisfy)

data NewGameReq = NewGameReq
  { ngrPlayer1Name :: !Text
  , ngrPlayer1Disc :: !Text
  , ngrStartingDisc :: !Text
  }

instance Aeson.ToJSON NewGameReq where
  toJSON NewGameReq{..} =
    object
      [ "_type" .= ("new" :: Text)
      , "player1Name" .= ngrPlayer1Name
      , "player1Disc" .= ngrPlayer1Disc
      , "startingDisc" .= ngrStartingDisc
      ]

data NewGame = NewGame
  { ngPlayer1Name :: !Text
  , ngPlayer1Disc :: !Text
  , ngGameId :: !Text
  }
  deriving (Show)

instance Eq NewGame where
  g1 == g2 =
    on (==) ngPlayer1Disc g1 g2
      && on (==) ngPlayer1Name g1 g2

instance Aeson.FromJSON NewGame where
  parseJSON = Aeson.withObject "NewGame" $ \o ->
    NewGame
      <$> (o .: "player1Name")
      <*> (o .: "player1Disc")
      <*> (o .: "gameId")

data JoinGame = JoinGame
  { jgGameId :: !Text
  , jgPlayer2Name :: !Text
  }

instance Aeson.ToJSON JoinGame where
  toJSON JoinGame{..} =
    object
      [ "_type" .= ("join" :: Text)
      , "player2Name" .= jgPlayer2Name
      , "gameId" .= jgGameId
      ]

withServer :: (Warp.Port -> IO ()) -> IO ()
withServer =
  Warp.withApplication
    (fmap ConnectFour.waiApp ConnectFour.newGameRepo)

connect :: Aeson.ToJSON a => Warp.Port -> a -> IO (Async ())
connect port msg = do
  lock <- MVar.newEmptyMVar
  a <-
    Async.async $
      let ws =
            WebSockets.runClient "127.0.0.1" port "/" $ \conn -> do
              WebSockets.sendTextData conn (Aeson.encode msg)
              forever $ do
                _ <- WebSockets.receive conn
                MVar.tryPutMVar lock ()
       in ws `onException` MVar.tryPutMVar lock ()
  MVar.takeMVar lock
  pure a

withConnections :: Aeson.ToJSON a => Warp.Port -> [a] -> IO b -> IO b
withConnections port games =
  let start = traverse (connect port) games
      end tasks = traverse_ Async.cancel tasks
   in bracket start end . const

spec :: Spec
spec = around withServer $ do
  manager <- runIO $ Client.newManager Client.defaultManagerSettings

  it "GET /heatlhcheck returns 204 with no body" $ \port -> do
    request <-
      let url = "http://localhost:" <> show port <> "/healthcheck"
       in Client.parseRequest url
    response <- Client.httpLbs request manager
    let status = Status.statusCode $ Client.responseStatus response
     in status `shouldBe` 204
    let body = Client.responseBody response
     in body `shouldBe` mempty

  it "GET /games returns an empty list when no games are open" $ \port -> do
    request <-
      let url = "http://localhost:" <> show port <> "/games"
       in Client.parseRequest url
    response <- Client.httpLbs request manager
    let status = Status.statusCode $ Client.responseStatus response
     in status `shouldBe` 200
    let headers = Client.responseHeaders response
     in headers `shouldContain` [("Content-Type", "application/json")]
    let body = Aeson.decode @Aeson.Value $ Client.responseBody response
     in body `shouldBe` Just [aesonQQ| [] |]

  it "GET /games returns open games when there are open connections" $ \port -> do
    let games =
          [ NewGameReq "player_1" "red" "yellow"
          , NewGameReq "player_2" "yellow" "yellow"
          , NewGameReq "player_3" "yellow" "red"
          , NewGameReq "player_4" "red" "red"
          ]
    withConnections port games $ do
      request <-
        let url = "http://localhost:" <> show port <> "/games"
         in Client.parseRequest url
      response <- Client.httpLbs request manager
      let status = Status.statusCode $ Client.responseStatus response
       in status `shouldBe` 200
      let headers = Client.responseHeaders response
       in headers `shouldContain` [("Content-Type", "application/json")]
      let body = fromMaybe [] . Aeson.decode $ Client.responseBody response
          expectedGames =
            games <&> \NewGameReq{..} ->
              NewGame ngrPlayer1Name ngrPlayer1Disc ""
          uniqueGameIds = length . nub $ fmap ngGameId body
       in do
            body `shouldMatchList` expectedGames
            uniqueGameIds `shouldBe` length games

  it "ignores trailing and leading white space in new game message" $ \port -> do
    let games = [NewGameReq "   player_1  " " red  " "  yellow "]
    withConnections port games $ do
      request <-
        let url = "http://localhost:" <> show port <> "/games"
         in Client.parseRequest url
      response <- Client.httpLbs request manager
      let status = Status.statusCode $ Client.responseStatus response
       in status `shouldBe` 200
      let headers = Client.responseHeaders response
       in headers `shouldContain` [("Content-Type", "application/json")]
      let body = fromMaybe [] . Aeson.decode $ Client.responseBody response
          expectedGames =
            games <&> \NewGameReq{..} ->
              NewGame (strip ngrPlayer1Name) (strip ngrPlayer1Disc) ""
          uniqueGameIds = length . nub $ fmap ngGameId body
       in do
            body `shouldMatchList` expectedGames
            uniqueGameIds `shouldBe` length games

  it "game is removed from open games when someone joins it" $ \port -> do
    let games =
          [ NewGameReq "player_1" "red" "yellow"
          , NewGameReq "player_2" "yellow" "yellow"
          , NewGameReq "player_3" "yellow" "red"
          , NewGameReq "player_4" "red" "red"
          ]
    withConnections port games $ do
      request <-
        let url = "http://localhost:" <> show port <> "/games"
         in Client.parseRequest url
      Just (ng : _) <-
        Aeson.decode @[NewGame] . Client.responseBody <$> Client.httpLbs request manager
      withConnections port [JoinGame (ngGameId ng) "other_player"] $ do
        Just lessGames <-
          Aeson.decode @[NewGame] . Client.responseBody <$> Client.httpLbs request manager
        let gameIds = fmap ngGameId lessGames
        gameIds `shouldNotSatisfy` elem (ngGameId ng)

  it "game is removed when disconnected" $ \port -> do
    let games =
          [ NewGameReq "player_1" "red" "yellow"
          , NewGameReq "player_2" "yellow" "yellow"
          , NewGameReq "player_4" "red" "red"
          ]
    withConnections port games $ do
      withConnections port [NewGameReq "other_player" "red" "red"] $ do
        request <-
          let url = "http://localhost:" <> show port <> "/games"
           in Client.parseRequest url
        Just firstCount <-
          fmap length . Aeson.decode @[NewGame] . Client.responseBody <$> Client.httpLbs request manager
        firstCount `shouldBe` 4
      request <-
        let url = "http://localhost:" <> show port <> "/games"
         in Client.parseRequest url
      Just secondCount <-
        fmap length . Aeson.decode @[NewGame] . Client.responseBody <$> Client.httpLbs request manager
      secondCount `shouldBe` 3
