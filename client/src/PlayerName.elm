module PlayerName exposing (PlayerName, PlayerNameError(..), make, toString)


type PlayerName
    = PlayerName String


toString : PlayerName -> String
toString (PlayerName playerName) =
    playerName


type PlayerNameError
    = PlayerNameIsEmpty


make : String -> Result PlayerNameError PlayerName
make playerName =
    let
        trimmedPlayerName =
            String.trim playerName
    in
    if String.isEmpty trimmedPlayerName then
        Err PlayerNameIsEmpty

    else
        Ok (PlayerName trimmedPlayerName)
