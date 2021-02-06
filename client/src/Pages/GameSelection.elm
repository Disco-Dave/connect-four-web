module Pages.GameSelection exposing
    ( Model
    , Msg
    , ParentMsg(..)
    , init
    , update
    , view
    )

import Browser
import Disc exposing (Disc(..))
import GameId exposing (GameId)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import PlayerName exposing (PlayerName)


type alias PendingGame =
    { gameId : GameId
    , player1Name : PlayerName
    , player1Disc : Disc
    }


type PendingGames
    = PendingGamesNotLoaded
    | PendingGamesLoaded (List PendingGame)
    | PendingGamesFailed Http.Error


isLoading : PendingGames -> Bool
isLoading pendingGames =
    case pendingGames of
        PendingGamesNotLoaded ->
            True

        _ ->
            False


type alias Model =
    { pendingGames : PendingGames
    , apiUrl : String
    }


type Msg
    = BackButtonClicked
    | NewGameButtonClicked
    | GotPendingGames (Result Http.Error (List PendingGame))
    | Refresh


type ParentMsg
    = GoBack
    | NewGame


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


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ParentMsg )
update msg model =
    case msg of
        GotPendingGames result ->
            let
                newPendingGames =
                    case result of
                        Err e ->
                            PendingGamesFailed e

                        Ok p ->
                            PendingGamesLoaded p
            in
            ( { model | pendingGames = newPendingGames }
            , Cmd.none
            , Nothing
            )

        Refresh ->
            ( { model | pendingGames = PendingGamesNotLoaded }
            , getPendingGames model.apiUrl
            , Nothing
            )

        BackButtonClicked ->
            ( model, Cmd.none, Just GoBack )

        NewGameButtonClicked ->
            ( model, Cmd.none, Just NewGame )


viewGame : PendingGame -> H.Html Msg
viewGame game =
    H.div [ A.class "pending-game" ]
        [ H.a
            [ A.class "pending-game__join", A.href (GameId.toString game.gameId) ]
            [ H.text "Join" ]
        , H.h2
            [ A.class "pending-game__player" ]
            [ H.text (PlayerName.toString game.player1Name) ]
        ]


viewGames : PendingGames -> List (H.Html Msg)
viewGames pendingGames =
    case pendingGames of
        PendingGamesNotLoaded ->
            []

        PendingGamesFailed _ ->
            [ H.p
                [ A.class "available-games__info" ]
                [ H.text "Failed to load games, please refresh to try again." ]
            ]

        PendingGamesLoaded loadedGames ->
            case loadedGames of
                [] ->
                    [ H.p
                        [ A.class "available-games__info" ]
                        [ H.text "No games are available, please make a new game. " ]
                    ]

                _ ->
                    loadedGames
                        |> List.sortBy (.player1Name >> PlayerName.toString)
                        |> List.map viewGame


view : Model -> Browser.Document Msg
view model =
    { title = "Connect Four - Select a game"
    , body =
        [ H.main_ [ A.class "main main--full-height" ]
            [ H.h1 [ A.class "title" ] [ H.text "Select a game" ]
            , H.div [ A.class "games" ]
                [ H.div
                    [ A.classList [ ( "available-games", True ), ( "loading", isLoading model.pendingGames ) ] ]
                    (viewGames model.pendingGames)
                , H.button
                    [ A.class "button button--danger"
                    , A.type_ "button"
                    , E.onClick BackButtonClicked
                    ]
                    [ H.text "Back" ]
                , H.button
                    [ A.class "button button--secondary"
                    , A.type_ "button"
                    , E.onClick Refresh
                    ]
                    [ H.text "Refresh" ]
                , H.button
                    [ A.class "button"
                    , A.type_ "button"
                    , E.onClick NewGameButtonClicked
                    ]
                    [ H.text "New game" ]
                ]
            ]
        ]
    }


init : String -> ( Model, Cmd Msg )
init apiUrl =
    ( { pendingGames = PendingGamesNotLoaded
      , apiUrl = apiUrl
      }
    , getPendingGames apiUrl
    )
