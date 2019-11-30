module Emojidojo.Error exposing (toString)

import Http exposing (Error(..))


toString : Error -> String
toString error =
    case error of
        BadUrl string ->
            "Bad Url: " ++ string

        Timeout ->
            "Timeout: The server has not responded."

        NetworkError ->
            "An error within the Network has occurred."

        BadStatus int ->
            "The response has a bad status: " ++ String.fromInt int ++ "."

        BadBody string ->
            "the body is illformed: " ++ string
