module ConnectFour.PlayerName (
  PlayerName,
  PlayerNameError (..),
  make,
  toText,
) where

import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Text (Text)
import qualified Data.Text as Text

data PlayerNameError = PlayerNameIsEmpty
  deriving (Show, Eq)

newtype PlayerName = PlayerName {fromPlayerName :: Text}
  deriving (Show, Eq, Ord, ToJSON) via Text

make :: Text -> Either PlayerNameError PlayerName
make (Text.strip -> playerName)
  | Text.null playerName = Left PlayerNameIsEmpty
  | otherwise = Right $ PlayerName playerName

toText :: PlayerName -> Text
toText = fromPlayerName

instance FromJSON PlayerName where
  parseJSON = withText "PlayerName" $ \text ->
    case make text of
      Right playerName -> pure playerName
      Left PlayerNameIsEmpty -> fail "PlayerName may not be mepty."
