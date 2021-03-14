module Pages.Game exposing
    ( Init(..)
    , Model
    , Msg
    , ParentMsg(..)
    , Player1Disc
    , StartingDisc
    , getGameId
    , init
    , subscriptions
    , update
    , view
    )

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
    | OtherPlayerLeft ConnectedGame


type ParentMsg
    = UpdateUrl GameId
    | GoBack


type Msg
    = ReceivedEvent (Result Decode.Error GameConnection.Event)
    | MovePlayed GameConnection.Move
    | LeaveButtonClicked


getGameId : Model -> Maybe GameId
getGameId model =
    case model of
        Pending (Join _ gameId) ->
            Just gameId

        Pending (New _ _ _) ->
            Nothing

        Single { gameId } ->
            Just gameId

        Connected { gameId } ->
            Just gameId

        OtherPlayerLeft { gameId } ->
            Just gameId


getBoard : Model -> GameConnection.Board
getBoard model =
    case model of
        Single { board } ->
            board

        Connected { board } ->
            board

        OtherPlayerLeft { board } ->
            board

        _ ->
            let
                emptyColumn =
                    { row1 = Nothing
                    , row2 = Nothing
                    , row3 = Nothing
                    , row4 = Nothing
                    , row5 = Nothing
                    , row6 = Nothing
                    }
            in
            { column1 = emptyColumn
            , column2 = emptyColumn
            , column3 = emptyColumn
            , column4 = emptyColumn
            , column5 = emptyColumn
            , column6 = emptyColumn
            , column7 = emptyColumn
            }


getDiscColor : Model -> Maybe Disc
getDiscColor model =
    case model of
        Pending _ ->
            Nothing

        Single { myDiscColor } ->
            Just myDiscColor

        Connected { myDiscColor } ->
            Just myDiscColor

        OtherPlayerLeft { myDiscColor } ->
            Just myDiscColor


getStatus : Model -> Maybe GameConnection.Status
getStatus model =
    case model of
        Pending _ ->
            Nothing

        Single { status } ->
            Just status

        Connected { status } ->
            Just status

        OtherPlayerLeft { status } ->
            Just status


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

        ReceivedEvent (Ok (GameConnection.PlayerLeft _)) ->
            let
                newModel =
                    case model of
                        Connected connected ->
                            OtherPlayerLeft connected

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

        LeaveButtonClicked ->
            ( model, GameConnection.close (), Just GoBack )

        _ ->
            ( model, Cmd.none, Nothing )


subscriptions : Sub Msg
subscriptions =
    Sub.map ReceivedEvent GameConnection.subscribe


viewBoard : GameConnection.Board -> H.Html Msg
viewBoard board =
    let
        viewSlot row column move =
            let
                disc =
                    board
                        |> column
                        |> row

                css =
                    A.classList
                        [ ( "board__slot", True )
                        , ( "board__slot--red", disc == Just Disc.RedDisc )
                        , ( "board__slot--yellow", disc == Just Disc.YellowDisc )
                        ]
            in
            H.button [ css, E.onClick (MovePlayed move) ] []

        viewRow row =
            [ viewSlot row .column1 GameConnection.DropIntoColumnOne
            , viewSlot row .column2 GameConnection.DropIntoColumnTwo
            , viewSlot row .column3 GameConnection.DropIntoColumnThree
            , viewSlot row .column4 GameConnection.DropIntoColumnFour
            , viewSlot row .column5 GameConnection.DropIntoColumnFive
            , viewSlot row .column6 GameConnection.DropIntoColumnSix
            , viewSlot row .column7 GameConnection.DropIntoColumnSeven
            ]

        boardView =
            [ .row6, .row5, .row4, .row3, .row2, .row1 ]
                |> List.concatMap viewRow
    in
    H.div [ A.class "board" ] boardView


view : Model -> Browser.Document Msg
view model =
    let
        board =
            getBoard model
                |> viewBoard

        title =
            case model of
                Pending _ ->
                    "Connecting..."

                Single _ ->
                    "Waiting for other player"

                OtherPlayerLeft game ->
                    PlayerName.toString game.otherPlayerName ++ " left"

                Connected game ->
                    "Playing with " ++ PlayerName.toString game.otherPlayerName

        yourDiscColor =
            case getDiscColor model of
                Nothing ->
                    H.text ""

                Just disc ->
                    H.h2 [ A.class "title title--small" ]
                        [ H.text "You are "
                        , case disc of
                            Disc.RedDisc ->
                                H.span [ A.class "red" ] [ H.text "Red" ]

                            Disc.YellowDisc ->
                                H.span [ A.class "yellow" ] [ H.text "Yellow" ]
                        ]

        status =
            case getStatus model of
                Just (GameConnection.WaitingFor disc) ->
                    H.h2 [ A.class "title title--small" ]
                        [ H.text "Waiting for "
                        , case disc of
                            Disc.RedDisc ->
                                H.span [ A.class "red" ] [ H.text "Red" ]

                            Disc.YellowDisc ->
                                H.span [ A.class "yellow" ] [ H.text "Yellow" ]
                        ]

                Just GameConnection.Tie ->
                    H.h2 [ A.class "title title--small" ]
                        [ H.text "You tied" ]

                Just (GameConnection.Win disc) ->
                    if Just disc == getDiscColor model then
                        H.h2 [ A.class "title title--small" ]
                            [ H.text "You won" ]

                    else
                        H.h2 [ A.class "title title--small" ]
                            [ H.text "You lost" ]

                Nothing ->
                    H.text ""
    in
    { title = "Connect Four - " ++ title
    , body =
        [ H.main_ [ A.class "main" ]
            [ H.div [ A.class "title-group" ]
                [ H.h1 [ A.class "title" ] [ H.text title ]
                , yourDiscColor
                , status
                ]
            , board
            , H.button
                [ A.class "button button--danger"
                , A.type_ "button"
                , E.onClick LeaveButtonClicked
                ]
                [ H.text "Leave" ]
            ]
        ]
    }
