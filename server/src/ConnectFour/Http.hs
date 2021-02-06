module ConnectFour.Http (waiApp) where

import ConnectFour.GameRepo (GameRepo, PendingGame (..), fakePendingGames, listGames)
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai as Wai

waiApp :: GameRepo -> Wai.Application
waiApp gameRepo request send =
  case (Wai.requestMethod request, Wai.pathInfo request) of
    ("GET", ["games"]) -> do
      games <- Aeson.encode <$> fakePendingGames
      let headers = [("Content-Type", "application/json")]
       in send $ Wai.responseLBS HttpTypes.ok200 headers games
    ("GET", ["healthcheck"]) ->
      send $ Wai.responseLBS HttpTypes.status204 [] mempty
    _ -> send $ Wai.responseLBS HttpTypes.notFound404 [] ""
