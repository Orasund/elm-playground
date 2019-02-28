module LittleWorldPuzzler.Request exposing
    ( Response(..)
    , getHighscore
    , setHighscore
    )

import Dict exposing (Dict)
import Http exposing (Body, Error(..), Expect)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry, gameVersion)


type Response
    = GotHighscore Entry
    | AchivedNewHighscore
    | GotError Error
    | Done


url : String
url =
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
                        AchivedNewHighscore

                Err error ->
                    case error of
                        BadBody _ ->
                            (error |> Debug.log "Error")
                                |> always AchivedNewHighscore

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
