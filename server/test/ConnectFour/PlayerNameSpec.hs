module ConnectFour.PlayerNameSpec (spec) where

import ConnectFour.PlayerName (PlayerNameError (..))
import qualified ConnectFour.PlayerName as PlayerName
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)

make :: Text -> Either PlayerNameError Text
make = fmap PlayerName.toText . PlayerName.make

spec :: Spec
spec = do
  it "does not allow empty text" $
    PlayerName.make "" `shouldBe` Left PlayerNameIsEmpty

  it "does not allow all whitespace" $
    PlayerName.make "        " `shouldBe` Left PlayerNameIsEmpty

  it "removes leading whitespace" $
    make "    Leading Whitespace" `shouldBe` Right "Leading Whitespace"

  it "removes trailing whitespace" $
    make "Trailing Whitespace    " `shouldBe` Right "Trailing Whitespace"

  it "removes both leading and trailing whitespace" $
    make "    Leading and Trailing Whitespace    " `shouldBe` Right "Leading and Trailing Whitespace"

  it "can be converted to JSON" $
    let playerName = PlayerName.make "player name"
     in fmap Aeson.encode playerName `shouldBe` Right "\"player name\""

  it "two of the same player names are equal to each other" $
    let playerName1 = PlayerName.make "player name "
        playerName2 = PlayerName.make " player name "
     in playerName1 `shouldBe` playerName2

  it "two different player names are not equal to each other" $
    let playerName1 = PlayerName.make "player name "
        playerName2 = PlayerName.make " player2 name "
     in playerName1 `shouldNotBe` playerName2
