module Pages.Game exposing (..)

import Browser
import Disc exposing (Disc(..))
import GameConnection
import GameId exposing (GameId)
import Html as H
import Html.Attributes as A
import Json.Decode as Decode
import PlayerName exposing (PlayerName)
import Route


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
    = ReceivedEvent (Result Decode.Error GameConnection.Event)


init : Init -> ( Model, Cmd Msg )
init initArgs =
    case initArgs of
        New player1Name player1Disc startingDisc ->
            ( { gameId = Nothing
              , player1Name = Just player1Name
              , player1Disc = Just player1Disc
              , player2Name = Nothing
              }
            , GameConnection.newGame
                { player1Name = player1Name
                , player1Disc = player1Disc
                , startingDisc = startingDisc
                }
            )

        Join player2Name gameId ->
            ( { gameId = Just gameId
              , player1Name = Nothing
              , player1Disc = Nothing
              , player2Name = Just player2Name
              }
            , GameConnection.joinGame
                { gameId = gameId
                , player2Name = player2Name
                }
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.map ReceivedEvent GameConnection.subscribe


view : Model -> Browser.Document Msg
view _ =
    { title = "Connect Four - New Game"
    , body =
        [ H.main_ [ A.class "main" ]
            [ H.h1 [ A.class "title" ] [ H.text "TODO" ]
            ]
        ]
    }
