module ConnectFour.GameSpec (spec) where

import ConnectFour.Game (Game, PlayError (..), Status (..))
import qualified ConnectFour.Game as Game
import ConnectFour.Game.Board (Column (..), Disc (..))
import qualified ConnectFour.Game.Board as Board
import ConnectFour.Game.ColumnStack (oppositeDisc)
import Data.Bifunctor (first)
import Data.Foldable (foldlM, for_)
import Test.Hspec (Spec, describe, it, shouldBe)

type Move = (Disc, Column)

runGame :: Move -> [Move] -> Either PlayError Game
runGame firstMove@(firstDisc, _) =
  let runMove g (disc, column) = Game.play disc column g
   in foldlM runMove (Game.newGame firstDisc) . (firstMove :)

spec :: Spec
spec = do
  describe "new game starts with correct WaitingFor status and empty board" $
    for_ [RedDisc, YellowDisc] $ \disc ->
      it ("starting with " <> show disc) $
        let game = Game.newGame disc
         in (Game.status game, Game.board game) `shouldBe` (WaitingFor disc, Board.emptyBoard)

  describe "cannot play out of turn" $ do
    for_ [RedDisc, YellowDisc] $ \disc ->
      it ("cannot play a " <> show disc <> " when waiting for " <> show (oppositeDisc disc)) $
        let game = Game.newGame (oppositeDisc disc)
         in Game.play disc Column1 game `shouldBe` Left OutOfTurn

  describe "cannot overfill a column" $
    for_ [Column1 ..] $ \column ->
      it (show column) $
        let game =
              runGame
                (RedDisc, column)
                [ (YellowDisc, column)
                , (RedDisc, column)
                , (YellowDisc, column)
                , (RedDisc, column)
                , (YellowDisc, column)
                , (RedDisc, column)
                ]
         in game `shouldBe` Left (ColumnIsFull column)

  describe "can detect a winner" $ do
    for_ [RedDisc, YellowDisc] $ \disc ->
      it (show disc) $
        let game =
              runGame
                (disc, Column1)
                [ (oppositeDisc disc, Column1)
                , (disc, Column2)
                , (oppositeDisc disc, Column2)
                , (disc, Column3)
                , (oppositeDisc disc, Column3)
                , (disc, Column4)
                ]
         in fmap Game.status game `shouldBe` Right (Win disc)

  it "can detect a tie" $
    let game =
          runGame
            (RedDisc, Column1)
            [ (YellowDisc, Column1)
            , (RedDisc, Column1)
            , (YellowDisc, Column1)
            , (RedDisc, Column1)
            , (YellowDisc, Column1)
            , (RedDisc, Column3)
            , (YellowDisc, Column3)
            , (RedDisc, Column3)
            , (YellowDisc, Column3)
            , (RedDisc, Column3)
            , (YellowDisc, Column3)
            , (RedDisc, Column5)
            , (YellowDisc, Column5)
            , (RedDisc, Column5)
            , (YellowDisc, Column5)
            , (RedDisc, Column5)
            , (YellowDisc, Column5)
            , (RedDisc, Column7)
            , (YellowDisc, Column7)
            , (RedDisc, Column7)
            , (YellowDisc, Column7)
            , (RedDisc, Column7)
            , (YellowDisc, Column7)
            , (RedDisc, Column2)
            , (YellowDisc, Column4)
            , (RedDisc, Column4)
            , (YellowDisc, Column4)
            , (RedDisc, Column4)
            , (YellowDisc, Column4)
            , (RedDisc, Column4)
            , (YellowDisc, Column2)
            , (RedDisc, Column2)
            , (YellowDisc, Column2)
            , (RedDisc, Column2)
            , (YellowDisc, Column2)
            , (RedDisc, Column6)
            , (YellowDisc, Column6)
            , (RedDisc, Column6)
            , (YellowDisc, Column6)
            , (RedDisc, Column6)
            , (YellowDisc, Column6)
            ]
     in fmap Game.status game `shouldBe` Right Tie

  describe "does not allow more pieces after game has ended" $ do
    it "after a winner has been declared" $
      let game =
            runGame
              (RedDisc, Column1)
              [ (YellowDisc, Column1)
              , (RedDisc, Column2)
              , (YellowDisc, Column2)
              , (RedDisc, Column3)
              , (YellowDisc, Column3)
              , (RedDisc, Column4)
              , (RedDisc, Column4)
              ]
       in game `shouldBe` Left GameIsOver

    it "after a tie has been declared" $
      let game =
            runGame
              (RedDisc, Column1)
              [ (YellowDisc, Column1)
              , (RedDisc, Column1)
              , (YellowDisc, Column1)
              , (RedDisc, Column1)
              , (YellowDisc, Column1)
              , (RedDisc, Column3)
              , (YellowDisc, Column3)
              , (RedDisc, Column3)
              , (YellowDisc, Column3)
              , (RedDisc, Column3)
              , (YellowDisc, Column3)
              , (RedDisc, Column5)
              , (YellowDisc, Column5)
              , (RedDisc, Column5)
              , (YellowDisc, Column5)
              , (RedDisc, Column5)
              , (YellowDisc, Column5)
              , (RedDisc, Column7)
              , (YellowDisc, Column7)
              , (RedDisc, Column7)
              , (YellowDisc, Column7)
              , (RedDisc, Column7)
              , (YellowDisc, Column7)
              , (RedDisc, Column2)
              , (YellowDisc, Column4)
              , (RedDisc, Column4)
              , (YellowDisc, Column4)
              , (RedDisc, Column4)
              , (YellowDisc, Column4)
              , (RedDisc, Column4)
              , (YellowDisc, Column2)
              , (RedDisc, Column2)
              , (YellowDisc, Column2)
              , (RedDisc, Column2)
              , (YellowDisc, Column2)
              , (RedDisc, Column6)
              , (YellowDisc, Column6)
              , (RedDisc, Column6)
              , (YellowDisc, Column6)
              , (RedDisc, Column6)
              , (YellowDisc, Column6)
              , (YellowDisc, Column6)
              ]
       in game `shouldBe` Left GameIsOver

  let expectWins moveSets =
        for_ (zip [1 :: Int ..] moveSets) $ \(index, (winner, firstMove, moves)) -> do
          it ("example " <> show index <> ": " <> show winner <> " as winner") $
            let game = runGame firstMove moves
             in fmap Game.status game `shouldBe` Right (Win winner)

          it ("example " <> show index <> ": " <> show (oppositeDisc winner) <> " as winner") $
            let game = runGame (first oppositeDisc firstMove) (fmap (first oppositeDisc) moves)
             in fmap Game.status game `shouldBe` Right (Win (oppositeDisc winner))
   in do
        describe "can detect horizontal wins" $
          expectWins
            [
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column2)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                , (YellowDisc, Column7)
                , (RedDisc, Column2)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column1)
                , (YellowDisc, Column7)
                , (RedDisc, Column2)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column5)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column2)
                , (YellowDisc, Column6)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column3)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column1)
                , (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column3)
                , (RedDisc, Column6)
                , (YellowDisc, Column4)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column1)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column6)
                , (YellowDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column6)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column6)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column2)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column7)
                , (RedDisc, Column5)
                , (YellowDisc, Column7)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column3)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column7)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column2)
                , (RedDisc, Column3)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column5)
                , (RedDisc, Column6)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column1)
                , (YellowDisc, Column6)
                , (RedDisc, Column2)
                , (YellowDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( YellowDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                ]
              )
            ]

        describe "can detect vertical wins" $
          expectWins
            [
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column1)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                , (YellowDisc, Column5)
                , (RedDisc, Column1)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column3)
                , (RedDisc, Column1)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                , (YellowDisc, Column7)
                , (RedDisc, Column1)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                , (YellowDisc, Column7)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column1)
                , (YellowDisc, Column4)
                , (RedDisc, Column1)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column6)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                , (YellowDisc, Column6)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column5)
                , (RedDisc, Column2)
                , (YellowDisc, Column4)
                , (RedDisc, Column2)
                , (YellowDisc, Column7)
                , (RedDisc, Column2)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column5)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column7)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column6)
                , (RedDisc, Column3)
                , (YellowDisc, Column6)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column3)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column2)
                , (RedDisc, Column4)
                , (YellowDisc, Column2)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column5)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column3)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column5)
              ,
                [ (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column6)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column5)
              ,
                [ (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column3)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                , (YellowDisc, Column3)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column6)
              ,
                [ (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                , (YellowDisc, Column3)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column7)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column7)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column6)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column5)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column6)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column5)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column7)
              ,
                [ (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column6)
                , (RedDisc, Column7)
                , (YellowDisc, Column4)
                , (RedDisc, Column7)
                , (YellowDisc, Column3)
                , (RedDisc, Column7)
                ]
              )
            ]

        describe "can detect forward diagonal wins" $
          expectWins
            [
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column1)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column1)
                , (RedDisc, Column1)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column2)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column2)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column7)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column7)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column7)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column2)
              ,
                [ (YellowDisc, Column2)
                , (RedDisc, Column2)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column7)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column1)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column7)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column3)
              ,
                [ (YellowDisc, Column3)
                , (RedDisc, Column3)
                , (YellowDisc, Column5)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column5)
                , (RedDisc, Column4)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column3)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column1)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column4)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column2)
                , (RedDisc, Column7)
                ]
              )
            ,
              ( RedDisc
              , (RedDisc, Column4)
              ,
                [ (YellowDisc, Column4)
                , (RedDisc, Column4)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column5)
                , (RedDisc, Column5)
                , (YellowDisc, Column1)
                , (RedDisc, Column5)
                , (YellowDisc, Column2)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column6)
                , (RedDisc, Column6)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                , (YellowDisc, Column7)
                , (RedDisc, Column7)
                ]
              )
            ]

        describe "can detect backward diagonal wins" $
          expectWins []
