module Pages.Game exposing (..)

import Browser
import Disc exposing (Disc(..))
import GameConnection
import GameId exposing (GameId)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import PlayerName exposing (PlayerName)


type alias StartingDisc =
    Disc


type alias Player1Disc =
    Disc


type Init
    = New PlayerName Player1Disc StartingDisc
    | Join PlayerName GameId


type alias SinglePlayerGame =
    { gameId : GameId
    , myDiscColor : Disc
    , status : GameConnection.Status
    , board : GameConnection.Board
    }


type alias ConnectedGame =
    { gameId : GameId
    , otherPlayerName : PlayerName
    , myDiscColor : Disc
    , status : GameConnection.Status
    , board : GameConnection.Board
    }


type Model
    = Pending Init
    | Single SinglePlayerGame
    | Connected ConnectedGame


type ParentMsg
    = UpdateUrl GameId


type Msg
    = ReceivedEvent (Result Decode.Error GameConnection.Event)
    | MovePlayed GameConnection.Move


getGameId : Model -> Maybe GameId
getGameId model =
    case model of
        Pending (Join _ gameId) ->
            Just gameId

        Single { gameId } ->
            Just gameId

        Connected { gameId } ->
            Just gameId

        _ ->
            Nothing


getBoard : Model -> Maybe GameConnection.Board
getBoard model =
    case model of
        Single { board } ->
            Just board

        Connected { board } ->
            Just board

        _ ->
            Nothing


init : Init -> ( Model, Cmd Msg )
init initArgs =
    let
        cmd =
            case initArgs of
                New player1Name player1Disc startingDisc ->
                    GameConnection.newGame
                        { player1Name = player1Name
                        , player1Disc = player1Disc
                        , startingDisc = startingDisc
                        }

                Join player2Name gameId ->
                    GameConnection.joinGame
                        { gameId = gameId
                        , player2Name = player2Name
                        }
    in
    ( Pending initArgs, cmd )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ParentMsg )
update msg model =
    case msg of
        ReceivedEvent (Ok (GameConnection.NewReceived gameId disc status board)) ->
            ( Single
                { gameId = gameId
                , myDiscColor = disc
                , status = status
                , board = board
                }
            , Cmd.none
            , Just (UpdateUrl gameId)
            )

        ReceivedEvent (Ok (GameConnection.JoinReceived gameId disc status board otherPlayerName)) ->
            ( Connected
                { gameId = gameId
                , myDiscColor = disc
                , status = status
                , board = board
                , otherPlayerName = otherPlayerName
                }
            , Cmd.none
            , Just (UpdateUrl gameId)
            )

        ReceivedEvent (Ok (GameConnection.PlayerJoined otherPlayerName)) ->
            let
                newModel =
                    case model of
                        Single singleModel ->
                            Connected
                                { gameId = singleModel.gameId
                                , myDiscColor = singleModel.myDiscColor
                                , status = singleModel.status
                                , board = singleModel.board
                                , otherPlayerName = otherPlayerName
                                }

                        _ ->
                            model
            in
            ( newModel, Cmd.none, Nothing )

        ReceivedEvent (Ok (GameConnection.GameUpdated status board)) ->
            let
                newModel =
                    case model of
                        Connected connected ->
                            Connected
                                { connected
                                    | status = status
                                    , board = board
                                }

                        _ ->
                            model
            in
            ( newModel, Cmd.none, Nothing )

        MovePlayed move ->
            let
                cmd =
                    case model of
                        Connected _ ->
                            GameConnection.sendMove move

                        _ ->
                            Cmd.none
            in
            ( model, cmd, Nothing )

        _ ->
            ( model, Cmd.none, Nothing )


subscriptions : Sub Msg
subscriptions =
    Sub.map ReceivedEvent GameConnection.subscribe


viewBoard : GameConnection.Board -> H.Html Msg
viewBoard board =
    let
        viewRow disc =
            let
                discColor =
                    case disc of
                        Just Disc.RedDisc ->
                            " board__row--red"

                        Just Disc.YellowDisc ->
                            " board__row--yellow"

                        _ ->
                            ""
            in
            H.div [ A.class ("board__row" ++ discColor) ] []

        viewColumn column move =
            H.button [ A.class "board__column", E.onClick (MovePlayed move) ]
                [ viewRow column.row6
                , viewRow column.row5
                , viewRow column.row4
                , viewRow column.row3
                , viewRow column.row2
                , viewRow column.row1
                ]
    in
    H.div [ A.class "board" ]
        [ viewColumn board.column1 GameConnection.DropIntoColumnOne
        , viewColumn board.column2 GameConnection.DropIntoColumnTwo
        , viewColumn board.column3 GameConnection.DropIntoColumnThree
        , viewColumn board.column4 GameConnection.DropIntoColumnFour
        , viewColumn board.column5 GameConnection.DropIntoColumnFive
        , viewColumn board.column6 GameConnection.DropIntoColumnSix
        , viewColumn board.column7 GameConnection.DropIntoColumnSeven
        ]


view : Model -> Browser.Document Msg
view model =
    let
        board =
            getBoard model
                |> Maybe.map viewBoard
                |> Maybe.withDefault (H.text "")
    in
    { title = "Connect Four - New Game"
    , body =
        [ H.main_ [ A.class "main" ]
            [ H.h1 [ A.class "title" ] [ H.text "TODO" ]
            , board
            ]
        ]
    }
