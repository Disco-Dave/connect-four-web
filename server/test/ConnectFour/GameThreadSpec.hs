module ConnectFour.GameThreadSpec where

import ConnectFour.Game.Board (Disc (..))
import ConnectFour.GameThread (Context (..), Event (..), GameThread)
import qualified ConnectFour.GameThread as GameThread
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket)
import Control.Monad (forM_, replicateM)
import Test.Hspec (Spec, around, describe, it, shouldBe)

threadCount :: Int
threadCount = 25

data Msg
  = IntMsg !Int
  | StringMsg !String
  deriving (Show, Eq)

withGameThread :: (GameThread Msg -> IO ()) -> IO ()
withGameThread = bracket (GameThread.start RedDisc) GameThread.kill

spec :: Spec
spec =
  around withGameThread $ do
    describe "can broadcast custom messages" $
      forM_ [StringMsg "test 1", StringMsg "test 2", IntMsg 22, IntMsg 55] $ \msg ->
        it ("message: " <> show msg) $ \gt -> do
          -- We must create the 'Context's before we fork new threads because
          -- the implementation uses a 'dupTChan', therefore we need to make
          -- sure the channel is duplicated before we broadcast a message.
          contexts <- replicateM threadCount (GameThread.createContext gt)
          tasks <- traverse (Async.async . contextReceive) contexts
          GameThread.broadcast msg gt
          events <- traverse Async.wait tasks
          events `shouldBe` replicate threadCount (Broadcasted msg)
