module Pages.GameSelection exposing
    ( Model
    , Msg(..)
    , ParentMsg(..)
    , init
    , update
    , view
    )

import Browser
import GameId exposing (GameId)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json
import PlayerName exposing (PlayerName)


type alias Model =
    { playerName : PlayerName
    }


init : PlayerName -> Model
init playerName =
    { playerName = playerName
    }


type Msg
    = BackButtonClicked


type ParentMsg
    = StartGame GameId
    | GoBack


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ParentMsg )
update msg model =
    case msg of
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
