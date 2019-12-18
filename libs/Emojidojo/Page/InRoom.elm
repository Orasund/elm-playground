module Emojidojo.Page.InRoom exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Game as Game exposing (Game)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.PlayerInfo as PlayerInfo exposing (PlayerInfo)
import Emojidojo.Data.Version as Version
import Emojidojo.Page.InGame as InGame
import Emojidojo.String as String
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading
import Http exposing (Error)
import Jsonstore exposing (Json)
import Random exposing (Seed)
import Task exposing (Task)
import Time exposing (Posix)


type alias Model =
    { roomId : Id
    , playerId : Id
    , activePlayers : List PlayerInfo
    , inactivePlayers : List PlayerInfo
    , hosting : Bool
    , message : Maybe String
    , error : Maybe Http.Error
    , lastUpdated : Posix
    , seed : Seed
    }


type Msg remote
    = PressedLeaveRoomButton
    | LeftRoom
    | TimePassed Posix
    | GotRoomResponse (Result Error (Maybe OpenRoom))
    | PressedStartGameButton
    | GotGameResponse (Result Error (Maybe (Game remote)))


type alias TransitionData =
    { room : OpenRoom
    , playerId : Id
    , hosting : Bool
    , lastUpdated : Posix
    , seed : Seed
    }


type Error
    = HttpError Http.Error
    | WrongVersion


type alias Action remote data =
    Action.Action Model (Msg remote) (InGame.TransitionData data) ()


initialModel : Config -> TransitionData -> Model
initialModel config { room, playerId, hosting, lastUpdated, seed } =
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
    , activePlayers = activePlayers
    , inactivePlayers = inactivePlayers
    , hosting = hosting
    , message = Nothing
    , error = Nothing
    , lastUpdated = lastUpdated
    , seed = seed
    }


init : Config -> TransitionData -> ( Model, Cmd (Msg remote) )
init config data =
    let
        model : Model
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


updateTask : Config -> Model -> Task Error (Maybe OpenRoom)
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
                OpenRoom.getResponse config model.roomId
                    |> Task.mapError HttpError
            )


update :
    { init : remote -> data
    , config : Config
    , json : Json remote
    , remoteInit : remote
    }
    -> Msg remote
    -> Model
    -> Action remote data
update input msg model =
    case msg of
        PressedLeaveRoomButton ->
            Action.updating <|
                ( { model | message = Just "Leaving..." }
                , PlayerInfo.removeResponse input.config
                    { roomId = model.roomId
                    , playerId = model.playerId
                    }
                    |> (if model.hosting then
                            Task.andThen
                                (always
                                    (OpenRoom.removeResponse input.config model.roomId)
                                )

                        else
                            identity
                       )
                    |> Task.attempt (always LeftRoom)
                )

        LeftRoom ->
            Action.exiting

        TimePassed lastUpdated ->
            let
                newModel =
                    { model
                        | message = Just <| "Synchronizing..."
                        , lastUpdated = lastUpdated
                    }
            in
            Action.updating
                ( newModel
                , updateTask input.config newModel
                    |> Task.attempt GotRoomResponse
                )

        GotRoomResponse result ->
            case result of
                Ok (Just room) ->
                    case room.gameId of
                        Just id ->
                            Action.updating
                                ( { model | message = Just "entering game..." }
                                , Game.getResponse input.config
                                    { gameId = id
                                    , jsonData = input.json
                                    }
                                    |> Task.mapError HttpError
                                    |> Task.attempt GotGameResponse
                                )

                        Nothing ->
                            Action.updating
                                ( initialModel input.config
                                    { room = room
                                    , playerId = model.playerId
                                    , hosting = model.hosting
                                    , lastUpdated = model.lastUpdated
                                    , seed = model.seed
                                    }
                                , Cmd.none
                                )

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
                , Game.insertResponse input.config
                    { dataJson = input.json
                    , game =
                        { id = gameId
                        , lastUpdated = model.lastUpdated
                        , player =
                            model.activePlayers
                                |> List.map
                                    (\player ->
                                        ( player.id, player )
                                    )
                                |> Dict.fromList
                        , data = input.remoteInit
                        , currentPlayer = model.playerId
                        }
                    , gameId = gameId
                    }
                    |> Task.mapError HttpError
                    |> Task.andThen
                        (\() ->
                            OpenRoom.insertGameIdResponse input.config
                                { roomId = model.roomId
                                , gameId = gameId
                                }
                                |> Task.mapError HttpError
                        )
                    |> Task.andThen (\() -> updateTask input.config model)
                    |> Task.attempt GotRoomResponse
                )

        GotGameResponse result ->
            case result of
                Ok (Just game) ->
                    Action.transitioning
                        { game = game |> Game.map (game.data |> input.init)
                        , playerId = model.playerId
                        , hosting = model.hosting
                        , lastUpdated = model.lastUpdated
                        , seed = model.seed
                        }

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


subscriptions : Model -> Sub (Msg remote)
subscriptions _ =
    Time.every (1000 * 5) TimePassed


view :
    Config
    -> Model
    ->
        { element : Element (Msg remote)
        , message : Maybe String
        , error : Maybe Http.Error
        }
view config { roomId, playerId, activePlayers, inactivePlayers, hosting, message, error } =
    { element =
        Element.column (Grid.simple ++ Card.fill) <|
            [ Element.row Grid.spacedEvenly <|
                [ Element.el Heading.h1 <|
                    Element.text <|
                        "Room Id: "
                            ++ Id.view roomId
                , Input.button (Button.simple ++ Color.danger)
                    { onPress = Just PressedLeaveRoomButton
                    , label =
                        Element.text <|
                            if hosting then
                                "close Room"

                            else
                                "leave Room"
                    }
                ]
            , activePlayers
                |> List.map
                    (\{ id } ->
                        Element.el Card.large <|
                            Element.text <|
                                Id.view <|
                                    id
                    )
                |> Element.wrappedRow Grid.simple
            , inactivePlayers
                |> List.map
                    (\{ id } ->
                        Element.el (Card.large ++ Color.disabled) <|
                            Element.text <|
                                Id.view <|
                                    id
                    )
                |> Element.wrappedRow Grid.simple
            , Element.row Grid.spacedEvenly <|
                [ if
                    hosting
                        && (activePlayers
                                |> List.length
                                |> (==) config.nrOfplayers
                           )
                  then
                    Input.button (Button.simple ++ Color.success)
                        { onPress = Just PressedStartGameButton
                        , label =
                            Element.text <| "start Game"
                        }

                  else
                    Element.text <|
                        String.fromInt config.nrOfplayers
                            ++ " players are needed"
                , Element.text <|
                    "Player Id: "
                        ++ Id.view playerId
                ]
            ]
    , message = message
    , error = error
    }
