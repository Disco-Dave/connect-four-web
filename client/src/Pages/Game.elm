module Pages.Game exposing (..)

import Browser
import Disc exposing (Disc(..))
import GameId exposing (GameId)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import PlayerName exposing (PlayerName)


type alias StartingDisc =
    Disc


type alias Player1Disc =
    Disc


type alias Player2Disc =
    Disc


type alias Player1Name =
    PlayerName


type alias Player2Name =
    PlayerName


type Init
    = New Player1Name Player1Disc StartingDisc
    | Join Player2Name GameId


type alias Model =
    { gameId : Maybe GameId
    , player1Name : Maybe Player1Name
    , player1Disc : Maybe Player1Disc
    , player2Name : Maybe Player2Name
    }


player2Disc : Model -> Maybe Player2Disc
player2Disc { player1Disc } =
    Maybe.map Disc.opposite player1Disc


type Msg
    = NoOp


init : Init -> ( Model, Cmd Msg )
init initArgs =
    case initArgs of
        New player1Name player1Disc startingDisc ->
            ( { gameId = Nothing
              , player1Name = Just player1Name
              , player1Disc = Just player1Disc
              , player2Name = Nothing
              }
            , Cmd.none
            )

        Join player2Name gameId ->
            ( { gameId = Just gameId
              , player1Name = Nothing
              , player1Disc = Nothing
              , player2Name = Just player2Name
              }
            , Cmd.none
            )
