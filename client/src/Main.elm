module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Pages.PlayerName as PlayerNamePage
import Url


type PageModel
    = PlayerNameModel PlayerNamePage.Model
    | OtherModel


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : PageModel
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PlayerNameMsg PlayerNamePage.Msg


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , page = PlayerNameModel PlayerNamePage.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( { model | url = url }
            , Cmd.none
            )

        ( PlayerNameMsg pageMsg, PlayerNameModel pageModel ) ->
            let
                ( newPageModel, playerName ) =
                    PlayerNamePage.update pageMsg pageModel

                newModel =
                    case playerName of
                        Nothing ->
                            { model | page = PlayerNameModel newPageModel }

                        Just _ ->
                            { model | page = OtherModel }
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        renderPage toMsg { title, body } =
            { title = title
            , body = List.map (H.map toMsg) body
            }
    in
    case model.page of
        PlayerNameModel m ->
            renderPage PlayerNameMsg (PlayerNamePage.view m)

        _ ->
            { title = "Connect Four - Todo"
            , body =
                [ H.h1 [ A.class "title" ] [ H.text "Todo" ]
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
