module Emojidojo.Page.Waiting exposing (Model, Msg, init, subscriptions, update, view)

import Action
import Element exposing (Element)
import Emojidojo.Data as Data
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Error
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
    }


type Msg
    = GotOpenRoomResponse (Result Error (List OpenRoom))
    | GotTime Posix
    | GotSeed Seed
    | PressedRetryButton
    | Reset (Result Error ())


type alias Action =
    Action.Action Model Msg SelectingRoom.TransitionData Never


init : () -> ( Model, Cmd Msg )
init _ =
    ( { error = Nothing
      , lastUpdated = Nothing
      , openRooms = Nothing
      , seed = Nothing
      }
    , Cmd.batch
        [ OpenRoom.getListResponse
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


update : Msg -> Model -> Action
update msg model =
    case msg of
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

        PressedRetryButton ->
            Action.updating
                ( model
                , Jsonstore.delete (Data.url ++ String.openRoom)
                    |> Task.attempt Reset
                )

        Reset result ->
            case result of
                Ok () ->
                    init () |> Action.updating

                Err error ->
                    Action.updating ( { model | error = Just error }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view :
    Model
    ->
        { element : Element Msg
        , message : Maybe String
        , error : Maybe ( Error, Msg )
        }
view { error } =
    case error of
        Nothing ->
            { element = Element.none
            , message = Just "Loading..."
            , error = Nothing
            }

        Just err ->
            { element = Element.none
            , message = Nothing
            , error = Just ( err, PressedRetryButton )
            }
