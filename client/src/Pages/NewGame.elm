module Pages.NewGame exposing
    ( Model
    , Msg
    , NewGame
    , ParentMsg(..)
    , init
    , update
    , view
    )

import Browser
import Disc exposing (Disc(..))
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json


type alias Model =
    { player1Disc : Disc
    , startingDisc : Disc
    }


init : Model
init =
    Model RedDisc RedDisc


type Msg
    = PlayerDiscChanged Disc
    | StartingDiscChanged Disc
    | BackButtonClicked
    | Submitted


type alias NewGame =
    { player1Disc : Disc
    , startingDisc : Disc
    }


type ParentMsg
    = GoBack
    | StartGame NewGame


update : Msg -> Model -> ( Model, Maybe ParentMsg )
update msg model =
    case msg of
        PlayerDiscChanged disc ->
            ( { model | player1Disc = disc }, Nothing )

        StartingDiscChanged disc ->
            ( { model | startingDisc = disc }, Nothing )

        BackButtonClicked ->
            ( model, Just GoBack )

        Submitted ->
            ( model, Just (StartGame model) )


onSubmit : msg -> H.Attribute msg
onSubmit msg =
    E.preventDefaultOn "submit" (Json.map (\_ -> ( msg, True )) (Json.succeed msg))


view : Model -> Browser.Document Msg
view model =
    { title = "Connect Four - New Game"
    , body =
        [ H.main_ [ A.class "main" ]
            [ H.h1 [ A.class "title" ] [ H.text "New Game" ]
            , H.form [ A.class "form", onSubmit Submitted ]
                [ H.div [ A.class "field" ]
                    [ H.label [ A.class "field__label", A.for "player1Disc" ] [ H.text "Your disc color" ]
                    , H.fieldset [ A.class "field__radios", A.id "player1Disc" ]
                        [ H.div [ A.class "radio" ]
                            [ H.input
                                [ A.type_ "radio"
                                , A.class "radio__input"
                                , A.name "player1Disc"
                                , A.id "player1Disc__red"
                                , A.checked (model.player1Disc == RedDisc)
                                , E.onCheck (\_ -> PlayerDiscChanged RedDisc)
                                ]
                                []
                            , H.label [ A.class "radio__label", A.for "player1Disc__red" ] [ H.text "Red" ]
                            ]
                        , H.div [ A.class "radio" ]
                            [ H.input
                                [ A.type_ "radio"
                                , A.class "radio__input"
                                , A.name "player1Disc"
                                , A.id "player1Disc__yellow"
                                , A.checked (model.player1Disc == YellowDisc)
                                , E.onCheck (\_ -> PlayerDiscChanged YellowDisc)
                                ]
                                []
                            , H.label [ A.class "radio__label", A.for "player1Disc__yellow" ] [ H.text "Yellow" ]
                            ]
                        ]
                    ]
                , H.div [ A.class "field" ]
                    [ H.label [ A.class "field__label", A.for "startingDisc" ] [ H.text "Starting disc color" ]
                    , H.fieldset [ A.class "field__radios", A.id "startingDisc" ]
                        [ H.div [ A.class "radio" ]
                            [ H.input
                                [ A.type_ "radio"
                                , A.class "radio__input"
                                , A.name "startingDisc"
                                , A.id "startingDisc__red"
                                , A.checked (model.startingDisc == RedDisc)
                                , E.onCheck (\_ -> StartingDiscChanged RedDisc)
                                ]
                                []
                            , H.label [ A.class "radio__label", A.for "startingDisc__red" ] [ H.text "Red" ]
                            ]
                        , H.div [ A.class "radio" ]
                            [ H.input
                                [ A.type_ "radio"
                                , A.class "radio__input"
                                , A.name "startingDisc"
                                , A.id "startingDisc__yellow"
                                , A.checked (model.startingDisc == YellowDisc)
                                , E.onCheck (\_ -> StartingDiscChanged YellowDisc)
                                ]
                                []
                            , H.label [ A.class "radio__label", A.for "startingDisc__yellow" ] [ H.text "Yellow" ]
                            ]
                        ]
                    ]
                , H.button
                    [ A.class "button button--danger", A.type_ "button", E.onClick BackButtonClicked ]
                    [ H.text "Back" ]
                , H.button [ A.class "button", A.type_ "submit" ] [ H.text "Start" ]
                ]
            ]
        ]
    }
