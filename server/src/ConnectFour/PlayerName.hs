module ConnectFour.PlayerName (
  PlayerName,
  PlayerNameError (..),
  make,
  toText,
) where

import Data.Aeson (ToJSON)
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
