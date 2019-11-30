module Emojidojo.Page.InGame exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.PlayerInfo as PlayerInfo exposing (PlayerInfo)
import Emojidojo.Data.Version as Version
import Emojidojo.String as String
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading
import Http exposing (Error)
import Random exposing (Seed)
import Task exposing (Task)
import Time exposing (Posix)


type alias Model data =
    { roomId : Id
    , playerId : Id
    , gameId : Id
    , activePlayers : List PlayerInfo
    , inactivePlayers : List PlayerInfo
    , hosting : Bool
    , message : Maybe String
    , error : Maybe Http.Error
    , lastUpdated : Posix
    , seed : Seed
    , gameData : data
    }


type Msg
    = PressedLeaveRoomButton
    | LeftRoom
    | TimePassed Posix
    | GotRoomResponse (Result Error (Maybe OpenRoom))
    | PressedStartGameButton


type alias TransitionData data =
    { room : OpenRoom
    , playerId : Id
    , gameId : Id
    , hosting : Bool
    , lastUpdated : Posix
    , seed : Seed
    , gameData : data
    }


type Error
    = HttpError Http.Error
    | WrongVersion


type alias Action data =
    Action.Action (Model data) Msg Never ()


initialModel : Config -> TransitionData data -> Model data
initialModel config { room, playerId, gameId, hosting, lastUpdated, seed, gameData } =
    let
        ( activePlayers, inactivePlayers ) =
            room.player
                |> Dict.values
                |> List.partition
                    (\list ->
                        (list.lastUpdated
                            |> Time.posixToMillis
                            |> (+) config.roomOpenInMillis
                        )
                            >= (lastUpdated
                                    |> Time.posixToMillis
                               )
                    )
    in
    { roomId = room.id
    , playerId = playerId
    , gameId = gameId
    , activePlayers = activePlayers
    , inactivePlayers = inactivePlayers
    , hosting = hosting
    , message = Nothing
    , error = Nothing
    , lastUpdated = lastUpdated
    , seed = seed
    , gameData = gameData
    }


init : Config -> TransitionData data -> ( Model data, Cmd Msg )
init config data =
    let
        model : Model data
        model =
            initialModel config data
    in
    ( { model | message = Just "joining room..." }
    , PlayerInfo.insertResponse config
        model.roomId
        { id = model.playerId
        , lastUpdated = model.lastUpdated
        }
        |> Task.mapError HttpError
        |> Task.andThen (\() -> updateTask config model)
        |> Task.attempt GotRoomResponse
    )


updateTask : Config -> Model data -> Task Error (Maybe OpenRoom)
updateTask config model =
    Version.getResponse config
        |> Task.mapError HttpError
        |> Task.andThen
            (\maybeFloat ->
                case maybeFloat of
                    Just float ->
                        if float == config.version then
                            Task.succeed ()

                        else
                            Task.fail WrongVersion

                    Nothing ->
                        Task.fail WrongVersion
            )
        |> Task.andThen
            (\() ->
                (if model.hosting then
                    OpenRoom.updateResponse config
                        { roomId = model.roomId
                        , lastUpdated = model.lastUpdated
                        }
                        |> Task.andThen
                            (\() ->
                                case model.inactivePlayers of
                                    player :: _ ->
                                        PlayerInfo.removeResponse config
                                            { roomId = model.roomId
                                            , playerId = player.id
                                            }

                                    _ ->
                                        Task.succeed ()
                            )

                 else
                    Task.succeed ()
                )
                    |> Task.mapError HttpError
            )
        |> Task.andThen
            (\() ->
                PlayerInfo.updateResponse config
                    { roomId = model.roomId
                    , playerId = model.playerId
                    , lastUpdated = model.lastUpdated
                    }
                    |> Task.mapError HttpError
            )
        |> Task.andThen
            (\() ->
                if
                    model.hosting
                        && (model.activePlayers |> List.length |> (/=) config.nrOfplayers)
                then
                    OpenRoom.removeResponse config model.roomId
                        |> Task.mapError HttpError

                else
                    Task.succeed ()
            )
        |> Task.andThen
            (\() ->
                OpenRoom.getResponse config model.roomId
                    |> Task.mapError HttpError
            )


update : Config -> Msg -> Model data -> Action data
update config msg model =
    case msg of
        PressedLeaveRoomButton ->
            Action.updating <|
                ( { model | message = Just "Leaving..." }
                , PlayerInfo.removeResponse config
                    { roomId = model.roomId
                    , playerId = model.playerId
                    }
                    |> (if model.hosting then
                            Task.andThen
                                (always
                                    (OpenRoom.removeResponse config model.roomId)
                                )

                        else
                            identity
                       )
                    |> Task.attempt (always LeftRoom)
                )

        LeftRoom ->
            Action.exiting

        TimePassed lastUpdated ->
            Action.updating
                ( { model
                    | message = Just <| "Synchronizing..."
                    , lastUpdated = lastUpdated
                  }
                , updateTask config model
                    |> Task.attempt GotRoomResponse
                )

        GotRoomResponse result ->
            case result of
                Ok (Just room) ->
                    case room.gameId of
                        Just id ->
                            Action.updating
                                ( initialModel config
                                    { room = room
                                    , playerId = model.playerId
                                    , hosting = model.hosting
                                    , lastUpdated = model.lastUpdated
                                    , seed = model.seed
                                    , gameData = model.gameData
                                    , gameId = id
                                    }
                                , Cmd.none
                                )

                        Nothing ->
                            Action.exiting

                Ok Nothing ->
                    Action.exiting

                Err (HttpError error) ->
                    Action.updating
                        ( { model
                            | error = Just error
                          }
                        , Cmd.none
                        )

                Err WrongVersion ->
                    Action.exiting

        PressedStartGameButton ->
            let
                ( gameId, seed ) =
                    model.seed
                        |> Random.step Id.generate
            in
            Action.updating <|
                ( { model | message = Just "game starting...", seed = seed }
                , OpenRoom.insertGameIdResponse config
                    { gameId = gameId
                    , roomId = model.roomId
                    }
                    |> Task.mapError HttpError
                    |> Task.andThen (\() -> updateTask config model)
                    |> Task.attempt GotRoomResponse
                )


subscriptions : Model data -> Sub Msg
subscriptions _ =
    Time.every (1000 * 5) TimePassed


view :
    { dataView : data -> Element msg
    , msgMapper : Msg -> msg
    }
    -> Model data
    ->
        { element : Element msg
        , message : Maybe String
        , error : Maybe Http.Error
        }
view { dataView, msgMapper } { gameData, hosting, message, error, gameId } =
    { element =
        Element.column (Grid.simple ++ Card.fill) <|
            [ Element.map msgMapper <|
                Element.row Grid.spacedEvenly <|
                    [ Element.el Heading.h1 <|
                        Element.text <|
                            "Game Id: "
                                ++ Id.view gameId
                    , Input.button (Button.simple ++ Color.danger)
                        { onPress = Just PressedLeaveRoomButton
                        , label =
                            Element.text <|
                                if hosting then
                                    "close Game"

                                else
                                    "leave Game"
                        }
                    ]
            , dataView gameData
            ]
    , message = message
    , error = error
    }
