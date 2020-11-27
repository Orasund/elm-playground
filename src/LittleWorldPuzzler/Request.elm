module LittleWorldPuzzler.Request exposing
    ( Response(..)
    , getHighscore
    , setHighscore
    )

import Firestore exposing (Document)
import Firestore.Codec as Codec
import Firestore.Encode exposing (Encoder)
import Http exposing (Error(..))
import LittleWorldPuzzler.Data as Data exposing (gameVersion)
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry)
import Task


type Response
    = GotHighscore Entry
    | AchievedNewHighscore
    | GotError Firestore.Error
    | Done


getHighscore : { score : Int, challenge : Bool } -> Cmd Response
getHighscore { score } =
    let
        response : Result Firestore.Error (Document Entry) -> Response
        response result =
            case result of
                Ok { fields } ->
                    if
                        fields.version
                            > gameVersion
                            || (fields.version == gameVersion && fields.score > score)
                    then
                        GotHighscore fields

                    else
                        AchievedNewHighscore

                Err error ->
                    case error of
                        Firestore.Http_ err ->
                            case err of
                                BadBody _ ->
                                    AchievedNewHighscore

                                _ ->
                                    error |> GotError

                        Firestore.Response _ ->
                            AchievedNewHighscore
    in
    Task.attempt
        response
        (Firestore.get
            (Entry.codec |> Codec.asDecoder)
            Data.firestore
        )


setHighscore : { entry : Entry, challenge : Bool } -> Cmd Response
setHighscore { entry } =
    let
        value : Encoder
        value =
            (Entry.codec |> Codec.asEncoder) entry

        response : Result Firestore.Error (Document Entry) -> Response
        response result =
            case result of
                Ok _ ->
                    Done

                Err error ->
                    error |> GotError
    in
    Task.attempt
        response
        (Firestore.override
            (Entry.codec |> Codec.asDecoder)
            value
            Data.firestore
        )
