module ConnectFour (
  newGameRepo,
  getPort,
  waiApp,
  start,
) where

import ConnectFour.GameRepo (GameRepo, newGameRepo)
import qualified ConnectFour.Http as Http
import qualified ConnectFour.WebSocket as WebSocket
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets.Connection (defaultConnectionOptions)
import qualified System.Environment as Env
import Text.Read (readMaybe)

getPort :: IO Port
getPort = do
  value <- (>>= readMaybe) <$> Env.lookupEnv "CONNECT_FOUR_WEB_PORT"
  pure $ fromMaybe 5123 value

waiApp :: GameRepo -> Wai.Application
waiApp repo =
  websocketsOr
    defaultConnectionOptions
    (WebSocket.serverApp repo)
    (Http.waiApp repo)

start :: GameRepo -> Port -> IO ()
start repo port =
  let beforeMainLoop = putStrLn $ "Now listening at: http://localhost:" <> show port
      settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setBeforeMainLoop beforeMainLoop
   in Warp.runSettings settings (waiApp repo)
