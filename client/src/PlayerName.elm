module PlayerName exposing
    ( PlayerName
    , PlayerNameError(..)
    , decoder
    , encode
    , fromString
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type PlayerName
    = PlayerName String


toString : PlayerName -> String
toString (PlayerName playerName) =
    playerName


type PlayerNameError
    = PlayerNameIsEmpty


fromString : String -> Result PlayerNameError PlayerName
fromString playerName =
    let
        trimmedPlayerName =
            String.trim playerName
    in
    if String.isEmpty trimmedPlayerName then
        Err PlayerNameIsEmpty

    else
        Ok (PlayerName trimmedPlayerName)


decoder : Decode.Decoder PlayerName
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case fromString s of
                    Ok playerName ->
                        Decode.succeed playerName

                    Err PlayerNameIsEmpty ->
                        Decode.fail "Player Name may not be emtpy"
            )


encode : PlayerName -> Encode.Value
encode =
    toString >> Encode.string
