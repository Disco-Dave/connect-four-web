module ConnectFour.Game.ColumnStack (
  Disc (..),
  ColumnStack,
  Row (..),
  discToText,
  textToDisc,
  oppositeDisc,
  emptyColumn,
  addDisc,
  size,
  lookup,
  ColumnView (..),
  view,
  isFull,
) where

import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), object, pairs, withText)
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Prelude hiding (lookup)
import qualified Prelude

data Disc
  = YellowDisc
  | RedDisc
  deriving (Show, Eq, Enum)

oppositeDisc :: Disc -> Disc
oppositeDisc YellowDisc = RedDisc
oppositeDisc RedDisc = YellowDisc

discToText :: Disc -> Text
discToText YellowDisc = "yellow"
discToText RedDisc = "red"

textToDisc :: Text -> Maybe Disc
textToDisc disc =
  case Text.map Char.toLower $ Text.strip disc of
    "red" -> Just RedDisc
    "yellow" -> Just YellowDisc
    _ -> Nothing

instance ToJSON Disc where
  toJSON = toJSON . discToText
  toEncoding = toEncoding . discToText

instance FromJSON Disc where
  parseJSON = withText "Disc" $ \value ->
    case textToDisc value of
      Just disc -> pure disc
      Nothing -> fail "Disc may only be \"yellow\" or \"red\""

newtype ColumnStack = ColumnStack {unsafeColumnStack :: [Disc]}
  deriving (Show, Eq) via [Disc]

data Row
  = Row1
  | Row2
  | Row3
  | Row4
  | Row5
  | Row6
  deriving (Show, Eq, Ord, Enum, Bounded)

emptyColumn :: ColumnStack
emptyColumn = ColumnStack []

addDisc :: Disc -> ColumnStack -> Maybe ColumnStack
addDisc disc stack
  | size stack >= 6 = Nothing
  | otherwise = Just . ColumnStack . (disc :) $ unsafeColumnStack stack

size :: ColumnStack -> Natural
size = fromIntegral . length . unsafeColumnStack

isFull :: ColumnStack -> Bool
isFull = (6 ==) . size

lookup :: Row -> ColumnStack -> Maybe Disc
lookup row (ColumnStack columns) =
  Prelude.lookup row $ zip [Row1 ..] (reverse columns)

data ColumnView = ColumnView
  { columnViewRow1 :: !(Maybe Disc)
  , columnViewRow2 :: !(Maybe Disc)
  , columnViewRow3 :: !(Maybe Disc)
  , columnViewRow4 :: !(Maybe Disc)
  , columnViewRow5 :: !(Maybe Disc)
  , columnViewRow6 :: !(Maybe Disc)
  }
  deriving (Show, Eq)

view :: ColumnStack -> ColumnView
view stack =
  ColumnView
    { columnViewRow1 = lookup Row1 stack
    , columnViewRow2 = lookup Row2 stack
    , columnViewRow3 = lookup Row3 stack
    , columnViewRow4 = lookup Row4 stack
    , columnViewRow5 = lookup Row5 stack
    , columnViewRow6 = lookup Row6 stack
    }

instance ToJSON ColumnView where
  toJSON ColumnView{..} =
    object
      [ "row1" .= columnViewRow1
      , "row2" .= columnViewRow2
      , "row3" .= columnViewRow3
      , "row4" .= columnViewRow4
      , "row5" .= columnViewRow5
      , "row6" .= columnViewRow6
      ]
  toEncoding ColumnView{..} =
    pairs . mconcat $
      [ "row1" .= columnViewRow1
      , "row2" .= columnViewRow2
      , "row3" .= columnViewRow3
      , "row4" .= columnViewRow4
      , "row5" .= columnViewRow5
      , "row6" .= columnViewRow6
      ]

instance ToJSON ColumnStack where
  toJSON = toJSON . view
  toEncoding = toEncoding . view
