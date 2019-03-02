module LittleWorldPuzzler.Request exposing
    ( Response(..)
    , getHighscore
    , setHighscore
    )

import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode exposing (Value)
import LittleWorldPuzzler.Data exposing (devMode, gameVersion)
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)


type Response
    = GotHighscore Entry
    | AchievedNewHighscore
    | GotError Error
    | Done


url : String
url =
    if devMode then
        "https://www.jsonstore.io/af03b4dff8d40b568fb0a32885b718ba9de323f3e68b395cc0980fa3e73e0e88/dev"

    else
        "https://www.jsonstore.io/af03b4dff8d40b568fb0a32885b718ba9de323f3e68b395cc0980fa3e73e0e88"


getHighscore : Int -> Cmd Response
getHighscore score =
    let
        response : Result Error Entry -> Response
        response result =
            case result of
                Ok entry ->
                    if
                        entry.version
                            > gameVersion
                            || (entry.version == gameVersion && entry.score > score)
                    then
                        GotHighscore entry

                    else
                        AchievedNewHighscore

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "Error")
                                |> always AchievedNewHighscore

                        _ ->
                            error |> GotError
    in
    Http.get
        { url = url ++ "/highscore"
        , expect = Http.expectJson response (D.field "result" <| Entry.decoder)
        }


setHighscore : Entry -> Cmd Response
setHighscore entry =
    let
        value : Value
        value =
            Entry.encode entry

        response : Result Error () -> Response
        response result =
            case result of
                Ok () ->
                    Done

                Err error ->
                    error |> GotError
    in
    Http.post
        { url = url ++ "/highscore"
        , body = Http.jsonBody value
        , expect = Http.expectWhatever response
        }
