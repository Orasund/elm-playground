module Emojidojo.Page.InRoom exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data as Data
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


type alias Model =
    { roomId : Id
    , playerId : Id
    , gameId : Maybe Id
    , activePlayers : List PlayerInfo
    , inactivePlayers : List PlayerInfo
    , hosting : Bool
    , message : Maybe String
    , error : Maybe Http.Error
    , lastUpdated : Posix
    , seed : Seed
    }


type Msg
    = PressedLeaveRoomButton
    | LeftRoom
    | TimePassed Posix
    | GotRoomResponse (Result Error (Maybe OpenRoom))
    | PressedStartGameButton


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


type alias Action =
    Action.Action Model Msg Never ()


initialModel : TransitionData -> Model
initialModel { room, playerId, hosting, lastUpdated, seed } =
    let
        ( activePlayers, inactivePlayers ) =
            room.player
                |> Dict.values
                |> List.partition
                    (\list ->
                        (list.lastUpdated
                            |> Time.posixToMillis
                            |> (+) Data.roomOpenInMillis
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
    , gameId = room.gameId
    }


init : TransitionData -> ( Model, Cmd Msg )
init data =
    let
        model : Model
        model =
            initialModel data
    in
    ( { model | message = Just "joining room..." }
    , PlayerInfo.insertResponse model.roomId
        { id = model.playerId
        , lastUpdated = model.lastUpdated
        }
        |> Task.mapError HttpError
        |> Task.andThen (\() -> updateTask model)
        |> Task.attempt GotRoomResponse
    )


updateTask : Model -> Task Error (Maybe OpenRoom)
updateTask model =
    Version.getResponse
        |> Task.mapError HttpError
        |> Task.andThen
            (\maybeFloat ->
                case maybeFloat of
                    Just float ->
                        if float == Data.version then
                            Task.succeed ()

                        else
                            Task.fail WrongVersion

                    Nothing ->
                        Task.fail WrongVersion
            )
        |> Task.andThen
            (\() ->
                (if model.hosting then
                    OpenRoom.updateResponse
                        { roomId = model.roomId
                        , lastUpdated = model.lastUpdated
                        }
                        |> Task.andThen
                            (\() ->
                                case model.inactivePlayers of
                                    player :: _ ->
                                        PlayerInfo.removeResponse
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
                PlayerInfo.updateResponse
                    { roomId = model.roomId
                    , playerId = model.playerId
                    , lastUpdated = model.lastUpdated
                    }
                    |> Task.mapError HttpError
            )
        |> Task.andThen
            (\() ->
                OpenRoom.getResponse model.roomId
                    |> Task.mapError HttpError
            )


update : Msg -> Model -> Action
update msg model =
    case msg of
        PressedLeaveRoomButton ->
            Action.updating <|
                ( { model | message = Just "Leaving..." }
                , PlayerInfo.removeResponse
                    { roomId = model.roomId
                    , playerId = model.playerId
                    }
                    |> (if model.hosting then
                            Task.andThen
                                (always
                                    (OpenRoom.removeResponse model.roomId)
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
                , updateTask model
                    |> Task.attempt GotRoomResponse
                )

        GotRoomResponse result ->
            case result of
                Ok (Just room) ->
                    Action.updating
                        ( initialModel
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
                , OpenRoom.insertGameIdResponse
                    { gameId = gameId
                    , roomId = model.roomId
                    }
                    |> Task.mapError HttpError
                    |> Task.andThen (\() -> updateTask model)
                    |> Task.attempt GotRoomResponse
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 * 5) TimePassed


view :
    Model
    ->
        { element : Element Msg
        , message : Maybe String
        , error : Maybe Http.Error
        }
view { roomId, playerId, activePlayers, inactivePlayers, hosting, message, error, gameId } =
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
                [ case gameId of
                    Just _ ->
                        Element.el [] <| Element.none

                    Nothing ->
                        if
                            hosting
                                && (activePlayers
                                        |> List.length
                                        |> (==) Data.nrOfplayers
                                   )
                        then
                            Input.button (Button.simple ++ Color.success)
                                { onPress = Just PressedStartGameButton
                                , label =
                                    Element.text <| "start Game"
                                }

                        else
                            Element.text <|
                                String.fromInt Data.nrOfplayers
                                    ++ " players are needed"
                , Element.text <|
                    "Player Id: "
                        ++ Id.view playerId
                ]
            ]
    , message = message
    , error = error
    }
