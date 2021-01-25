module ConnectFour.IntegrationSpec where

import qualified ConnectFour
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Status as Status
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec (Spec, around, it, shouldBe)

withServer :: (Warp.Port -> IO ()) -> IO ()
withServer =
  Warp.withApplication
    (fmap ConnectFour.waiApp ConnectFour.newGameRepo)

spec :: Spec
spec = around withServer $
  it "GET /heatlhcheck returns 204 with no body" $ \port -> do
    manager <- Client.newManager Client.defaultManagerSettings
    request <-
      let url = "http://localhost:" <> show port <> "/healthcheck"
       in Client.parseRequest url
    response <- Client.httpLbs request manager
    let status = Status.statusCode $ Client.responseStatus response
     in status `shouldBe` 204
    let body = Client.responseBody response
     in body `shouldBe` mempty
