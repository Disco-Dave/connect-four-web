module ConnectFour.Game (
  Status (..),
  Game,
  board,
  status,
  isOver,
  StartingDisc,
  newGame,
  PlayError (..),
  play,
) where

import ConnectFour.Game.Board (Board, Column (..), Disc (..), Row (..))
import qualified ConnectFour.Game.Board as Board
import ConnectFour.Game.ColumnStack (oppositeDisc)
import Data.Aeson (ToJSON)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Generics (Generic)

data Status
  = WaitingFor !Disc
  | Tie
  | Win !Disc
  deriving (Show, Eq, Generic)

instance ToJSON Status

data Game = Game
  { gameBoard :: !Board
  , gameStatus :: !Status
  }
  deriving (Show, Eq)

type StartingDisc = Disc

newGame :: StartingDisc -> Game
newGame startingDisc =
  Game
    { gameBoard = Board.emptyBoard
    , gameStatus = WaitingFor startingDisc
    }

board :: Game -> Board
board = gameBoard

status :: Game -> Status
status = gameStatus

isOver :: Game -> Bool
isOver Game{..} =
  case gameStatus of
    WaitingFor _ -> False
    _ -> True

data PlayError
  = ColumnIsFull !Column
  | OutOfTurn
  | GameIsOver
  deriving (Show, Eq)

chunkBy4 :: [a] -> [[a]]
chunkBy4 = go []
 where
  go chunks remaining =
    case remaining of
      c1 : c2 : c3 : c4 : rest ->
        go ([c1, c2, c3, c4] : chunks) (c2 : c3 : c4 : rest)
      _ -> chunks

rangesToCheck :: [[(Column, Row)]]
rangesToCheck =
  let verticals =
        mconcat
          [ chunkBy4 $ (column,) <$> enumFrom Row1
          | column <- [Column1 ..]
          ]
      horizontals =
        mconcat
          [ chunkBy4 $ (,row) <$> enumFrom Column1
          | row <- [Row1 ..]
          ]
      forwardDiagonals =
        nub . mconcat $
          [ chunkBy4 $ zip [column ..] [row ..]
          | row <- [Row1 ..]
          , column <- [Column1 ..]
          ]
      backwardsDiagonals =
        nub . mconcat $
          [ chunkBy4 $ zip [column, pred column ..] [row ..]
          | column <- [Column2 ..]
          , row <- [Row1 ..]
          ]
   in verticals <> horizontals <> forwardDiagonals <> backwardsDiagonals

findWinner :: Board -> Maybe Disc
findWinner board' =
  let areAllTheSameDisc rangeToCheck =
        case (\(c, r) -> Board.lookup c r board') <$> rangeToCheck of
          Just disc : rest@(_ : _) | all (Just disc ==) rest -> Just disc
          _ -> Nothing
   in listToMaybe $ mapMaybe areAllTheSameDisc rangesToCheck

newStatus :: Disc -> Board -> Status
newStatus lastPlayedDisc board' =
  case findWinner board' of
    Just disc -> Win disc
    Nothing
      | Board.isFull board' -> Tie
      | otherwise -> WaitingFor $ oppositeDisc lastPlayedDisc

play :: Disc -> Column -> Game -> Either PlayError Game
play disc column Game{..} =
  case gameStatus of
    Tie -> Left GameIsOver
    Win _ -> Left GameIsOver
    WaitingFor waitingForDisc
      | waitingForDisc /= disc -> Left OutOfTurn
      | otherwise ->
        case Board.addDisc disc column gameBoard of
          Left Board.ColumnIsFull ->
            Left $ ColumnIsFull column
          Right newBoard ->
            Right $ Game newBoard (newStatus disc newBoard)
