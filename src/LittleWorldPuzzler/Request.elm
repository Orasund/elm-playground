module LittleWorldPuzzler.Request exposing
    ( Response(..)
    , getHighscore
    , setHighscore
    )

import Http exposing (Error(..))
import Json.Encode exposing (Value)
import Jsonstore
import LittleWorldPuzzler.Data exposing (devMode, gameVersion)
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)
import Task


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


getHighscore : { score : Int, challenge : Bool } -> Cmd Response
getHighscore { score, challenge } =
    let
        response : Result Error (Maybe Entry) -> Response
        response result =
            case result of
                Ok maybeEntry ->
                    case maybeEntry of
                        Just entry ->
                            if
                                entry.version
                                    > gameVersion
                                    || (entry.version == gameVersion && entry.score > score)
                            then
                                GotHighscore entry

                            else
                                AchievedNewHighscore

                        Nothing ->
                            AchievedNewHighscore

                Err error ->
                    case error of
                        BadBody _ ->
                            error
                                |> always AchievedNewHighscore

                        _ ->
                            error |> GotError
    in
    Task.attempt
        response
        (Jsonstore.get
            (url
                ++ (if challenge then
                        "/challenge"

                    else
                        "/highscore"
                   )
            )
            (Jsonstore.decode <| Entry.json)
        )


setHighscore : { entry : Entry, challenge : Bool } -> Cmd Response
setHighscore { entry, challenge } =
    let
        value : Value
        value =
            (Entry.json |> Jsonstore.encode) entry

        response : Result Error () -> Response
        response result =
            case result of
                Ok () ->
                    Done

                Err error ->
                    error |> GotError
    in
    Task.attempt
        response
        (Jsonstore.insert
            (url
                ++ (if challenge then
                        "/challenge"

                    else
                        "/highscore"
                   )
            )
            value
        )
