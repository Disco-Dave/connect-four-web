module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H
import Pages.GameSelection as GameSelectionPage
import Pages.PlayerName as PlayerNamePage
import Url


type PageModel
    = PlayerNameModel PlayerNamePage.Model
    | GameSelectionModel GameSelectionPage.Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , apiUrl : String
    , page : PageModel
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PlayerNameMsg PlayerNamePage.Msg
    | GameSelectionMsg GameSelectionPage.Msg


type alias Flags =
    String


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , apiUrl = flags
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

                ( newModel, cmd ) =
                    case playerName of
                        Nothing ->
                            ( { model | page = PlayerNameModel newPageModel }
                            , Cmd.none
                            )

                        Just name ->
                            let
                                ( gameSelectionModel, gameCmd ) =
                                    GameSelectionPage.init model.apiUrl name
                            in
                            ( { model | page = GameSelectionModel gameSelectionModel }
                            , Cmd.map GameSelectionMsg gameCmd
                            )
            in
            ( newModel, cmd )

        ( GameSelectionMsg pageMsg, GameSelectionModel pageModel ) ->
            let
                ( newPageModel, pageCmd, parentMsg ) =
                    GameSelectionPage.update pageMsg pageModel

                newPage =
                    case parentMsg of
                        Just GameSelectionPage.GoBack ->
                            PlayerNamePage.init
                                |> PlayerNameModel

                        _ ->
                            GameSelectionModel newPageModel
            in
            ( { model | page = newPage }
            , Cmd.map GameSelectionMsg pageCmd
            )

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

        GameSelectionModel m ->
            renderPage GameSelectionMsg (GameSelectionPage.view m)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
