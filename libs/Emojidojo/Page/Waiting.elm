module Emojidojo.Page.Waiting exposing (Model, Msg, init, subscriptions, update, view)

import Action
import Element exposing (Element)
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.Version as Version
import Emojidojo.Page.SelectingRoom as SelectingRoom
import Emojidojo.String as String
import Http
import Jsonstore
import Random exposing (Seed)
import Task exposing (Task)
import Time exposing (Posix)


type alias Model =
    { openRooms : Maybe (List OpenRoom)
    , lastUpdated : Maybe Posix
    , seed : Maybe Seed
    , error : Maybe Http.Error
    , message : Maybe String
    }


type Error
    = HttpError Http.Error
    | WrongVersion Float


type Msg
    = GotOpenRoomResponse (Result Error (List OpenRoom))
    | GotTime Posix
    | GotSeed Seed


type alias Action =
    Action.Action Model Msg SelectingRoom.TransitionData Never


init : Config -> () -> ( Model, Cmd Msg )
init config _ =
    ( { error = Nothing
      , lastUpdated = Nothing
      , openRooms = Nothing
      , seed = Nothing
      , message = Just <| "Loading..."
      }
    , Cmd.batch
        [ updateTask config
            |> Task.attempt GotOpenRoomResponse
        , Time.now |> Task.perform GotTime
        , Random.generate GotSeed Random.independentSeed
        ]
    )


evaluate : Model -> Action
evaluate ({ openRooms, lastUpdated, seed } as model) =
    case ( openRooms, lastUpdated, seed ) of
        ( Just o, Just l, Just s ) ->
            { openRooms = o
            , lastUpdated = l
            , seed = s
            }
                |> Action.transitioning

        _ ->
            Action.updating ( model, Cmd.none )


updateTask : Config -> Task Error (List OpenRoom)
updateTask config =
    Version.getResponse config
        |> Task.mapError HttpError
        |> Task.andThen
            (\maybeFloat ->
                case maybeFloat of
                    Just float ->
                        if float == config.version then
                            OpenRoom.getListResponse config
                                |> Task.mapError HttpError

                        else
                            Task.fail (WrongVersion float)

                    Nothing ->
                        Version.insertResponse config
                            |> Task.andThen
                                (\() ->
                                    OpenRoom.getListResponse config
                                )
                            |> Task.mapError HttpError
            )


update : Config -> Msg -> Model -> Action
update config msg model =
    case msg of
        GotOpenRoomResponse result ->
            case result of
                Ok maybeList ->
                    { model | openRooms = Just maybeList }
                        |> evaluate

                Err error ->
                    case error of
                        HttpError err ->
                            Action.updating ( { model | error = Just err }, Cmd.none )

                        WrongVersion float ->
                            Action.updating <|
                                if float > config.version then
                                    ( { model
                                        | message =
                                            Just <|
                                                "You are running version "
                                                    ++ String.fromFloat config.version
                                                    ++ ". The current version is "
                                                    ++ String.fromFloat float
                                                    ++ ". Please refresh the page in order to upgrade to the new version."
                                      }
                                    , Cmd.none
                                    )

                                else
                                    ( { model | message = Just "updating..." }
                                    , Jsonstore.delete (Data.url config)
                                        |> Task.mapError HttpError
                                        |> Task.andThen (\() -> updateTask config)
                                        |> Task.attempt GotOpenRoomResponse
                                    )

        GotTime posix ->
            evaluate <| { model | lastUpdated = Just posix }

        GotSeed seed ->
            evaluate <| { model | seed = Just seed }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view :
    Model
    ->
        { element : Element Msg
        , message : Maybe String
        , error : Maybe Http.Error
        }
view { error, message } =
    { element = Element.none
    , message = message
    , error = error
    }
