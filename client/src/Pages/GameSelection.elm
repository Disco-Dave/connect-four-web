module Pages.GameSelection exposing
    ( Model
    , Msg(..)
    , ParentMsg(..)
    , init
    , update
    , view
    )

import Browser
import Disc exposing (Disc)
import GameId exposing (GameId)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import PlayerName exposing (PlayerName)


type alias Model =
    { playerName : PlayerName
    , isLoading : Bool
    }


type alias PendingGame =
    { gameId : GameId
    , player1Name : PlayerName
    , player1Disc : Disc
    }


type Msg
    = BackButtonClicked
    | GotPendingGames (Result Http.Error (List PendingGame))


getPendingGames : String -> Cmd Msg
getPendingGames apiUrl =
    let
        pendingGame =
            Decode.map3
                PendingGame
                (Decode.field "gameId" GameId.decoder)
                (Decode.field "player1Name" PlayerName.decoder)
                (Decode.field "player1Disc" Disc.decoder)
    in
    Http.get
        { url = apiUrl ++ "/games"
        , expect = Http.expectJson GotPendingGames (Decode.list pendingGame)
        }


init : String -> PlayerName -> ( Model, Cmd Msg )
init apiUrl playerName =
    ( { playerName = playerName
      , isLoading = True
      }
    , getPendingGames apiUrl
    )


type ParentMsg
    = StartGame GameId
    | GoBack


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ParentMsg )
update msg model =
    case msg of
        GotPendingGames _ ->
            ( { model | isLoading = False }
            , Cmd.none
            , Nothing
            )

        BackButtonClicked ->
            ( model, Cmd.none, Just GoBack )


view : Model -> Browser.Document Msg
view _ =
    { title = "Connect Four - Select a game"
    , body =
        [ H.h1 [ A.class "title" ] [ H.text "Select a game" ]
        , H.div [ A.class "games" ]
            [ H.table
                [ A.class "available-games" ]
                []
            , H.button
                [ A.class "button button--danger"
                , A.type_ "button"
                , E.onClick BackButtonClicked
                ]
                [ H.text "Back" ]
            , H.button
                [ A.class "button"
                , A.type_ "button"
                ]
                [ H.text "New game" ]
            ]
        ]
    }
