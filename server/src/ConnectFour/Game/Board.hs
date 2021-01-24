module ConnectFour.Game.Board (
  Board (..),
  Disc (..),
  Row (..),
  Column (..),
  emptyBoard,
  ColumnIsFull (..),
  addDisc,
  BoardView (..),
  view,
  isFull,
  lookup,
) where

import ConnectFour.Game.ColumnStack (ColumnStack, ColumnView, Disc (..), Row (..))
import qualified ConnectFour.Game.ColumnStack as ColumnStack
import Data.Aeson (KeyValue (..), ToJSON (toEncoding, toJSON), object, pairs)
import Prelude hiding (lookup)

data Board = Board
  { boardColumn1 :: !ColumnStack
  , boardColumn2 :: !ColumnStack
  , boardColumn3 :: !ColumnStack
  , boardColumn4 :: !ColumnStack
  , boardColumn5 :: !ColumnStack
  , boardColumn6 :: !ColumnStack
  , boardColumn7 :: !ColumnStack
  }
  deriving (Show, Eq)

data Column
  = Column1
  | Column2
  | Column3
  | Column4
  | Column5
  | Column6
  | Column7
  deriving (Show, Eq, Ord, Enum, Bounded)

emptyBoard :: Board
emptyBoard =
  Board
    { boardColumn1 = ColumnStack.emptyColumn
    , boardColumn2 = ColumnStack.emptyColumn
    , boardColumn3 = ColumnStack.emptyColumn
    , boardColumn4 = ColumnStack.emptyColumn
    , boardColumn5 = ColumnStack.emptyColumn
    , boardColumn6 = ColumnStack.emptyColumn
    , boardColumn7 = ColumnStack.emptyColumn
    }

data ColumnIsFull = ColumnIsFull
  deriving (Show, Eq)

addDisc :: Disc -> Column -> Board -> Either ColumnIsFull Board
addDisc disc column board =
  let update getStack setStack =
        case ColumnStack.addDisc disc $ getStack board of
          Nothing -> Left ColumnIsFull
          Just newStack -> Right $ setStack newStack
   in case column of
        Column1 -> update boardColumn1 (\s -> board{boardColumn1 = s})
        Column2 -> update boardColumn2 (\s -> board{boardColumn2 = s})
        Column3 -> update boardColumn3 (\s -> board{boardColumn3 = s})
        Column4 -> update boardColumn4 (\s -> board{boardColumn4 = s})
        Column5 -> update boardColumn5 (\s -> board{boardColumn5 = s})
        Column6 -> update boardColumn6 (\s -> board{boardColumn6 = s})
        Column7 -> update boardColumn7 (\s -> board{boardColumn7 = s})

lookup :: Column -> Row -> Board -> Maybe Disc
lookup column row Board{..} =
  case column of
    Column1 -> ColumnStack.lookup row boardColumn1
    Column2 -> ColumnStack.lookup row boardColumn2
    Column3 -> ColumnStack.lookup row boardColumn3
    Column4 -> ColumnStack.lookup row boardColumn4
    Column5 -> ColumnStack.lookup row boardColumn5
    Column6 -> ColumnStack.lookup row boardColumn6
    Column7 -> ColumnStack.lookup row boardColumn7

isFull :: Board -> Bool
isFull Board{..} =
  let columns =
        [ boardColumn1
        , boardColumn2
        , boardColumn3
        , boardColumn4
        , boardColumn5
        , boardColumn6
        , boardColumn7
        ]
   in all ColumnStack.isFull columns

data BoardView = BoardView
  { boardViewColumn1 :: !ColumnView
  , boardViewColumn2 :: !ColumnView
  , boardViewColumn3 :: !ColumnView
  , boardViewColumn4 :: !ColumnView
  , boardViewColumn5 :: !ColumnView
  , boardViewColumn6 :: !ColumnView
  , boardViewColumn7 :: !ColumnView
  }
  deriving (Show, Eq)

view :: Board -> BoardView
view Board{..} =
  BoardView
    { boardViewColumn1 = ColumnStack.view boardColumn1
    , boardViewColumn2 = ColumnStack.view boardColumn2
    , boardViewColumn3 = ColumnStack.view boardColumn3
    , boardViewColumn4 = ColumnStack.view boardColumn4
    , boardViewColumn5 = ColumnStack.view boardColumn5
    , boardViewColumn6 = ColumnStack.view boardColumn6
    , boardViewColumn7 = ColumnStack.view boardColumn7
    }

boardViewToKV :: KeyValue kv => BoardView -> [kv]
boardViewToKV BoardView{..} =
  [ "column1" .= boardViewColumn1
  , "column2" .= boardViewColumn2
  , "column3" .= boardViewColumn3
  , "column4" .= boardViewColumn4
  , "column5" .= boardViewColumn5
  , "column6" .= boardViewColumn6
  , "column7" .= boardViewColumn7
  ]

instance ToJSON BoardView where
  toJSON = object . boardViewToKV
  toEncoding = pairs . mconcat . boardViewToKV

instance ToJSON Board where
  toJSON = toJSON . view
  toEncoding = toEncoding . view
