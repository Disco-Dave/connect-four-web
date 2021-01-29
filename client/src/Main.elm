module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, for, id, name, type_)
import Html.Events exposing (..)
import Json.Decode as Json
import Url


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Submitted


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        Submitted ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onSubmit : msg -> Attribute msg
onSubmit msg =
    preventDefaultOn "submit" (Json.map alwaysPreventDefault (Json.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


view : Model -> Browser.Document Msg
view _ =
    { title = "Connect Four"
    , body =
        [ h1 [ class "title" ] [ text "Pick a name" ]
        , form [ class "form", onSubmit Submitted ]
            [ div [ class "field" ]
                [ label [ class "field__label", for "name" ] [ text "Name" ]
                , input [ class "field__input", type_ "text", id "name", name "name" ] []
                ]
            , button [ class "button", type_ "submit" ] [ text "Next" ]
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
