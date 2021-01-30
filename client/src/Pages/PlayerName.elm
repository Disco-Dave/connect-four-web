module Pages.PlayerName exposing (Model, Msg(..), init, update, view)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json
import PlayerName exposing (PlayerName, PlayerNameError(..))


type alias Model =
    { playerName : String
    , error : Maybe String
    }


init : Model
init =
    Model "" Nothing


type Msg
    = PlayerNameChanged String
    | PlayerNameBlurred
    | Submitted


update : Msg -> Model -> ( Model, Maybe PlayerName )
update msg model =
    case msg of
        PlayerNameChanged newPlayerName ->
            ( { model | playerName = newPlayerName }
            , Nothing
            )

        PlayerNameBlurred ->
            ( { model | playerName = String.trim model.playerName }
            , Nothing
            )

        Submitted ->
            let
                playerName =
                    PlayerName.make model.playerName

                error =
                    case playerName of
                        Err PlayerNameIsEmpty ->
                            Just "Name may not be empty"

                        Ok _ ->
                            Nothing
            in
            ( { model | error = error }
            , Result.toMaybe playerName
            )


onSubmit : msg -> H.Attribute msg
onSubmit msg =
    E.preventDefaultOn "submit" (Json.map (\_ -> ( msg, True )) (Json.succeed msg))


view : Model -> Browser.Document Msg
view model =
    let
        ( extraFieldClasses, feedback ) =
            case model.error of
                Nothing ->
                    ( [], [] )

                Just error ->
                    ( [ A.class "field--invalid" ]
                    , [ H.p [ A.class "field__feedback" ] [ H.text error ] ]
                    )
    in
    { title = "Connect Four - Pick a name"
    , body =
        [ H.h1 [ A.class "title" ] [ H.text "Pick a name" ]
        , H.form [ A.class "form", onSubmit Submitted ]
            [ H.div (A.class "field" :: extraFieldClasses)
                ([ H.label [ A.class "field__label", A.for "name" ] [ H.text "Name" ]
                 , H.input
                    [ A.class "field__input"
                    , A.type_ "text"
                    , A.id "name"
                    , A.name "name"
                    , A.value model.playerName
                    , E.onInput PlayerNameChanged
                    , E.onBlur PlayerNameBlurred
                    ]
                    []
                 ]
                    ++ feedback
                )
            , H.button [ A.class "button", A.type_ "submit" ] [ H.text "Next" ]
            ]
        ]
    }
