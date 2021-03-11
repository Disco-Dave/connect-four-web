module GameConnection exposing
    ( Board
    , Column
    , Event(..)
    , JoinGame
    , Move(..)
    , NewGame
    , Status(..)
    , close
    , joinGame
    , newGame
    , sendMove
    , subscribe
    )

import Disc exposing (Disc)
import GameId exposing (GameId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import PlayerName exposing (PlayerName)
import Ports


type alias NewGame =
    { player1Name : PlayerName
    , player1Disc : Disc
    , startingDisc : Disc
    }


newGame : NewGame -> Cmd msg
newGame options =
    Ports.connect <|
        Encode.object
            [ ( "_type", Encode.string "new" )
            , ( "player1Name", PlayerName.encode options.player1Name )
            , ( "player1Disc", Disc.encode options.player1Disc )
            , ( "startingDisc", Disc.encode options.startingDisc )
            ]


type alias JoinGame =
    { gameId : GameId
    , player2Name : PlayerName
    }


joinGame : JoinGame -> Cmd msg
joinGame options =
    Ports.connect <|
        Encode.object
            [ ( "_type", Encode.string "join" )
            , ( "gameId", GameId.encode options.gameId )
            , ( "player2Name", PlayerName.encode options.player2Name )
            ]


type Move
    = DropIntoColumnOne
    | DropIntoColumnTwo
    | DropIntoColumnThree
    | DropIntoColumnFour
    | DropIntoColumnFive
    | DropIntoColumnSix
    | DropIntoColumnSeven


sendMove : Move -> Cmd msg
sendMove move =
    Ports.send <|
        case move of
            DropIntoColumnOne ->
                Encode.int 1

            DropIntoColumnTwo ->
                Encode.int 2

            DropIntoColumnThree ->
                Encode.int 3

            DropIntoColumnFour ->
                Encode.int 4

            DropIntoColumnFive ->
                Encode.int 5

            DropIntoColumnSix ->
                Encode.int 6

            DropIntoColumnSeven ->
                Encode.int 7


type alias Column =
    { row1 : Maybe Disc
    , row2 : Maybe Disc
    , row3 : Maybe Disc
    , row4 : Maybe Disc
    , row5 : Maybe Disc
    , row6 : Maybe Disc
    }


columnDecoder : Decoder Column
columnDecoder =
    let
        row n =
            Decode.field
                ("row" ++ String.fromInt n)
                (Decode.nullable Disc.decoder)
    in
    Decode.map6 Column
        (row 1)
        (row 2)
        (row 3)
        (row 4)
        (row 5)
        (row 6)


type alias Board =
    { column1 : Column
    , column2 : Column
    , column3 : Column
    , column4 : Column
    , column5 : Column
    , column6 : Column
    , column7 : Column
    }


boardDecoder : Decoder Board
boardDecoder =
    let
        column n =
            Decode.field ("column" ++ String.fromInt n) columnDecoder
    in
    Decode.map7 Board
        (column 1)
        (column 2)
        (column 3)
        (column 4)
        (column 5)
        (column 6)
        (column 7)


type Status
    = WaitingFor Disc
    | Tie
    | Win Disc


statusDecoder : Decoder Status
statusDecoder =
    Decode.field "_type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "waitingFor" ->
                        Decode.map WaitingFor (Decode.field "disc" Disc.decoder)

                    "win" ->
                        Decode.map Win (Decode.field "disc" Disc.decoder)

                    "tie" ->
                        Decode.succeed Tie

                    _ ->
                        Decode.fail "Unrecognized \"_type\" for Status"
            )


type Event
    = PlayerJoined PlayerName
    | PlayerLeft PlayerName
    | GameUpdated Status Board
    | GameOver
    | NewReceived GameId Disc Status Board
    | JoinFailed
    | JoinReceived GameId Disc Status Board PlayerName


eventDecoder : Decoder Event
eventDecoder =
    Decode.oneOf
        [ Decode.field "_type" Decode.string
            |> Decode.andThen
                (\type_ ->
                    case type_ of
                        "playerJoined" ->
                            Decode.map PlayerJoined (Decode.field "playerName" PlayerName.decoder)

                        "playerLeft" ->
                            Decode.map PlayerLeft (Decode.field "playerName" PlayerName.decoder)

                        "gameUpdated" ->
                            Decode.map2
                                GameUpdated
                                (Decode.field "status" statusDecoder)
                                (Decode.field "board" boardDecoder)

                        "gameOver" ->
                            Decode.succeed GameOver

                        "newReceived" ->
                            Decode.map4
                                NewReceived
                                (Decode.field "gameId" GameId.decoder)
                                (Decode.field "yourDiscColor" Disc.decoder)
                                (Decode.field "status" statusDecoder)
                                (Decode.field "board" boardDecoder)

                        "joinReceived" ->
                            Decode.map5
                                JoinReceived
                                (Decode.field "gameId" GameId.decoder)
                                (Decode.field "yourDiscColor" Disc.decoder)
                                (Decode.field "status" statusDecoder)
                                (Decode.field "board" boardDecoder)
                                (Decode.field "otherPlayerName" PlayerName.decoder)

                        _ ->
                            Decode.fail "Unrecognized \"_type\" for Status"
                )
        , Decode.string
            |> Decode.andThen
                (\str ->
                    if str == "Game not found." then
                        Decode.succeed JoinFailed

                    else
                        Decode.fail "Unrecognized message"
                )
        ]


subscribe : Sub (Result Decode.Error Event)
subscribe =
    Ports.receive (Decode.decodeValue eventDecoder)


close : () -> Cmd msg
close =
    Ports.close
