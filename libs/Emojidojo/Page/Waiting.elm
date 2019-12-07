module Emojidojo.Page.Waiting exposing (Model, Msg, init, subscriptions, update, view)

import Action
import Element exposing (Element)
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Game as Game exposing (Game)
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.Version as Version
import Emojidojo.Page.SelectingRoom as SelectingRoom
import Emojidojo.String as String
import Http
import Jsonstore exposing (Json)
import Random exposing (Seed)
import Task exposing (Task)
import Time exposing (Posix)


type alias Model data =
    { openRooms : Maybe (List OpenRoom)
    , games : Maybe (List (Game data))
    , lastUpdated : Maybe Posix
    , seed : Maybe Seed
    , error : Maybe Http.Error
    , message : Maybe String
    }


type Error
    = HttpError Http.Error
    | WrongVersion Float


type Msg data
    = GotResponse (Result Error ( List OpenRoom, List (Game data) ))
    | GotTime Posix
    | GotSeed Seed


type alias Action data =
    Action.Action (Model data) (Msg data) (SelectingRoom.TransitionData data) Never


init : Json data -> Config -> () -> ( Model data, Cmd (Msg data) )
init jsonData config _ =
    ( { error = Nothing
      , lastUpdated = Nothing
      , openRooms = Nothing
      , games = Nothing
      , seed = Nothing
      , message = Just <| "Loading..."
      }
    , Cmd.batch
        [ updateTask jsonData config |> Task.attempt GotResponse
        , Time.now |> Task.perform GotTime
        , Random.generate GotSeed Random.independentSeed
        ]
    )


evaluate : Model data -> Action data
evaluate ({ games, openRooms, lastUpdated, seed } as model) =
    case ( games, ( openRooms, lastUpdated, seed ) ) of
        ( Just g, ( Just o, Just l, Just s ) ) ->
            { openRooms = o
            , games = g
            , lastUpdated = l
            , seed = s
            }
                |> Action.transitioning

        _ ->
            Action.updating ( model, Cmd.none )


updateTask : Json data -> Config -> Task Error ( List OpenRoom, List (Game data) )
updateTask jsonData config =
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
        |> Task.andThen
            (\maybeRoomList ->
                Game.getListResponse config jsonData
                    |> Task.mapError HttpError
                    |> Task.map (Tuple.pair maybeRoomList)
            )


update : Json data -> Config -> Msg data -> Model data -> Action data
update jsonData config msg model =
    case msg of
        GotResponse result ->
            case result of
                Ok ( maybeRoomList, maybeGameList ) ->
                    { model
                        | openRooms = Just maybeRoomList
                        , games = Just maybeGameList
                    }
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
                                        |> Task.andThen (\() -> updateTask jsonData config)
                                        |> Task.attempt GotResponse
                                    )

        GotTime posix ->
            evaluate <| { model | lastUpdated = Just posix }

        GotSeed seed ->
            evaluate <| { model | seed = Just seed }


subscriptions : Model data -> Sub (Msg data)
subscriptions _ =
    Sub.none


view :
    Model data
    ->
        { element : Element (Msg data)
        , message : Maybe String
        , error : Maybe Http.Error
        }
view { error, message } =
    { element = Element.none
    , message = message
    , error = error
    }
