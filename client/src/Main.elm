module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import GameConnection
import GameId exposing (GameId)
import Html
import Pages.Game
import Pages.GameSelection
import Pages.NewGame
import Pages.PlayerName
import PlayerName exposing (PlayerName)
import Route exposing (BasePath, Route)
import Url exposing (Url)


type alias Flags =
    { apiUrl : String
    , basePath : String
    }


type Page
    = GamePage Pages.Game.Model
    | GameSelectionPage Pages.GameSelection.Model
    | NewGamePage Pages.NewGame.Model
    | PlayerNamePage Pages.PlayerName.Model


type alias Model =
    { page : Page
    , route : Route
    , playerName : Maybe PlayerName
    , apiUrl : String
    , basePath : String
    , navKey : Navigation.Key
    }


type PageMsg
    = GameMsg Pages.Game.Msg
    | GameSelectionMsg Pages.GameSelection.Msg
    | NewGameMsg Pages.NewGame.Msg
    | PlayerNameMsg Pages.PlayerName.Msg


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | ReceivedPageMsg PageMsg


toRoute : BasePath -> Url -> Route
toRoute basePath =
    Route.parse basePath >> Maybe.withDefault Route.Home


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { page = PlayerNamePage Pages.PlayerName.init
      , route = toRoute flags.basePath url
      , playerName = Nothing
      , apiUrl = flags.apiUrl
      , basePath = flags.basePath
      , navKey = navKey
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    let
        renderPage toMsg { title, body } =
            { title = title
            , body = List.map (Html.map (toMsg >> ReceivedPageMsg)) body
            }
    in
    case model.page of
        PlayerNamePage m ->
            renderPage PlayerNameMsg (Pages.PlayerName.view m)

        GameSelectionPage m ->
            renderPage GameSelectionMsg (Pages.GameSelection.view model.basePath m)

        NewGamePage m ->
            renderPage NewGameMsg (Pages.NewGame.view m)

        GamePage m ->
            renderPage GameMsg (Pages.Game.view m)


joinGame : GameId -> PlayerName -> ( Page, Cmd Msg )
joinGame gameId playerName =
    let
        ( gameModel, gameCmd ) =
            Pages.Game.Join playerName gameId
                |> Pages.Game.init
    in
    ( GamePage gameModel
    , Cmd.map (GameMsg >> ReceivedPageMsg) gameCmd
    )


updatePage : PageMsg -> Model -> ( Model, Cmd Msg )
updatePage pageMsg model =
    let
        toModel toMainModel pageModel =
            { model | page = toMainModel pageModel }

        toCmd toPageMsg =
            Cmd.map (toPageMsg >> ReceivedPageMsg)
    in
    case ( pageMsg, model.page ) of
        ( GameMsg gameMsg, GamePage gameModel ) ->
            let
                ( newGameModel, gameCmd, parentMsg ) =
                    Pages.Game.update gameMsg gameModel

                extraCmds =
                    case parentMsg of
                        Nothing ->
                            []

                        Just (Pages.Game.UpdateUrl gameId) ->
                            [ Route.push model.navKey model.basePath (Route.Game gameId) ]

                        Just Pages.Game.GoBack ->
                            [ Route.push model.navKey model.basePath Route.Home ]
            in
            ( toModel GamePage newGameModel
            , Cmd.batch (toCmd GameMsg gameCmd :: extraCmds)
            )

        ( GameSelectionMsg gameSelectionMsg, GameSelectionPage gameSelectionPage ) ->
            let
                ( newGameSelectionModel, newGameSelectionCmd, parentMsg ) =
                    Pages.GameSelection.update model.apiUrl gameSelectionMsg gameSelectionPage
            in
            case parentMsg of
                Just Pages.GameSelection.GoBack ->
                    ( { model
                        | playerName = Nothing
                        , page = PlayerNamePage Pages.PlayerName.init
                      }
                    , toCmd GameSelectionMsg newGameSelectionCmd
                    )

                Just Pages.GameSelection.NewGame ->
                    ( { model | page = NewGamePage Pages.NewGame.init }
                    , toCmd GameSelectionMsg newGameSelectionCmd
                    )

                Nothing ->
                    ( toModel GameSelectionPage newGameSelectionModel
                    , toCmd GameSelectionMsg newGameSelectionCmd
                    )

        ( NewGameMsg newGameMsg, NewGamePage newGamePage ) ->
            let
                ( newNewGameModel, parentMsg ) =
                    Pages.NewGame.update newGameMsg newGamePage
            in
            case ( parentMsg, model.playerName ) of
                ( Just Pages.NewGame.GoBack, _ ) ->
                    let
                        ( gameSelectionModel, gameSelectionCmd ) =
                            Pages.GameSelection.init model.apiUrl
                    in
                    ( toModel GameSelectionPage gameSelectionModel
                    , toCmd GameSelectionMsg gameSelectionCmd
                    )

                ( Just (Pages.NewGame.StartGame newGame), Just playerName ) ->
                    let
                        ( gameModel, gameCmd ) =
                            Pages.Game.New playerName newGame.player1Disc newGame.startingDisc
                                |> Pages.Game.init
                    in
                    ( { model
                        | page = GamePage gameModel
                      }
                    , toCmd GameMsg gameCmd
                    )

                _ ->
                    ( toModel NewGamePage newNewGameModel
                    , Cmd.none
                    )

        ( PlayerNameMsg playerNameMsg, PlayerNamePage playerNamePage ) ->
            let
                ( newPlayerNameModel, playerName ) =
                    Pages.PlayerName.update playerNameMsg playerNamePage
            in
            case ( playerName, model.route ) of
                ( Nothing, _ ) ->
                    ( toModel PlayerNamePage newPlayerNameModel
                    , Cmd.none
                    )

                ( Just newPlayerName, Route.Home ) ->
                    let
                        ( gameSelectionModel, gameSelectionCmd ) =
                            Pages.GameSelection.init model.apiUrl
                    in
                    ( { model
                        | playerName = Just newPlayerName
                        , page = GameSelectionPage gameSelectionModel
                      }
                    , toCmd GameSelectionMsg gameSelectionCmd
                    )

                ( Just newPlayerName, Route.Game gameId ) ->
                    let
                        ( page, cmd ) =
                            joinGame gameId newPlayerName
                    in
                    ( { model
                        | playerName = Just newPlayerName
                        , page = page
                      }
                    , cmd
                    )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedPageMsg pageMsg ->
            updatePage pageMsg model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            let
                route =
                    toRoute model.basePath url

                currentGameId =
                    case model.page of
                        GamePage game ->
                            Pages.Game.getGameId game

                        _ ->
                            Nothing

                ( page, cmd ) =
                    case ( route, model.playerName ) of
                        ( Route.Game gameId, Just playerName ) ->
                            if Just gameId /= currentGameId then
                                joinGame gameId playerName

                            else
                                ( model.page, Cmd.none )

                        ( Route.Home, Just _ ) ->
                            let
                                ( gameSelectionModel, gameSelectionCmd ) =
                                    Pages.GameSelection.init model.apiUrl
                            in
                            ( GameSelectionPage gameSelectionModel
                            , Cmd.batch
                                [ GameConnection.close ()
                                , Cmd.map (GameSelectionMsg >> ReceivedPageMsg) gameSelectionCmd
                                ]
                            )

                        ( Route.Home, Nothing ) ->
                            ( PlayerNamePage Pages.PlayerName.init
                            , GameConnection.close ()
                            )

                        _ ->
                            ( model.page, Cmd.none )
            in
            ( { model
                | route = toRoute model.basePath url
                , page = page
              }
            , cmd
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map (GameMsg >> ReceivedPageMsg) Pages.Game.subscriptions


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
