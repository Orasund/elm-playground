module Emojidojo.Page.Waiting exposing (Model, Msg, init, subscriptions, update, view)

import Action
import Element exposing (Element)
import Emojidojo.Data as Data
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.Version as Version
import Emojidojo.Page.SelectingRoom as SelectingRoom
import Emojidojo.String as String
import Http exposing (Error(..))
import Jsonstore
import Random exposing (Seed)
import Task
import Time exposing (Posix)


type alias Model =
    { openRooms : Maybe (List OpenRoom)
    , lastUpdated : Maybe Posix
    , seed : Maybe Seed
    , error : Maybe Error
    , message : Maybe String
    }


type Msg
    = GotOpenRoomResponse (Result Error (List OpenRoom))
    | GotTime Posix
    | GotSeed Seed
    | GotVersion (Result Error (Maybe Float))


type alias Action =
    Action.Action Model Msg SelectingRoom.TransitionData Never


init : () -> ( Model, Cmd Msg )
init _ =
    ( { error = Nothing
      , lastUpdated = Nothing
      , openRooms = Nothing
      , seed = Nothing
      , message = Just <| "Loading..."
      }
    , Cmd.batch
        [ Version.getResponse
            |> Task.attempt GotVersion
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


update : Msg -> Model -> Action
update msg model =
    case msg of
        GotVersion result ->
            case result of
                Ok (Just float) ->
                    Action.updating <|
                        if float > Data.version then
                            ( { model
                                | message =
                                    Just <|
                                        "You are running version "
                                            ++ String.fromFloat Data.version
                                            ++ ". The current version is "
                                            ++ String.fromFloat float
                                            ++ ". Please refresh the page in order to upgrade to the new version."
                              }
                            , Cmd.none
                            )

                        else if float < Data.version then
                            ( { model | message = Just "updating..." }
                            , Jsonstore.delete Data.url
                                |> Task.andThen (\() -> Version.getResponse)
                                |> Task.attempt GotVersion
                            )

                        else
                            ( model
                            , OpenRoom.getListResponse
                                |> Task.attempt GotOpenRoomResponse
                            )

                Ok Nothing ->
                    Action.updating
                        ( { model | message = Just "updating..." }
                        , Version.insertResponse
                            |> Task.andThen (\() -> Version.getResponse)
                            |> Task.attempt GotVersion
                        )

                Err error ->
                    Action.updating ( { model | error = Just error }, Cmd.none )

        GotOpenRoomResponse result ->
            case result of
                Ok maybeList ->
                    { model | openRooms = Just maybeList }
                        |> evaluate

                Err error ->
                    Action.updating ( { model | error = Just error }, Cmd.none )

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
        , error : Maybe Error
        }
view { error, message } =
    { element = Element.none
    , message = message
    , error = error
    }
