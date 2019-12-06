module Emojidojo.Page.InGame exposing (Model, Msg(..), TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Game as Game exposing (Game)
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
import Jsonstore exposing (Json)
import Random exposing (Seed)
import Task exposing (Task)
import Time exposing (Posix)


type alias Model data =
    { roomId : Id
    , playerId : Id
    , game : Game data
    , activePlayers : List PlayerInfo
    , inactivePlayers : List PlayerInfo
    , hosting : Bool
    , message : Maybe String
    , error : Maybe Http.Error
    , lastUpdated : Posix
    , seed : Seed
    }


type Msg data msg
    = PressedLeaveRoomButton
    | LeftRoom
    | TimePassed Posix
    | GotRoomResponse (Result Error (Maybe (OpenRoom data)))
    | GameSpecific msg


type alias TransitionData data =
    { room : OpenRoom data
    , game : Game data
    , playerId : Id
    , hosting : Bool
    , lastUpdated : Posix
    , seed : Seed
    }


type Error
    = HttpError Http.Error
    | WrongVersion


type alias Action data msg =
    Action.Action (Model data) (Msg data msg) Never ()


initialModel : Config -> TransitionData data -> Model data
initialModel config { room, game, playerId, hosting, lastUpdated, seed } =
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
    , game = game
    , activePlayers = activePlayers
    , inactivePlayers = inactivePlayers
    , hosting = hosting
    , message = Nothing
    , error = Nothing
    , lastUpdated = lastUpdated
    , seed = seed
    }


init : Json data -> Config -> TransitionData data -> ( Model data, Cmd (Msg data msg) )
init json config data =
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
        |> Task.andThen (\() -> updateTask json config model)
        |> Task.attempt GotRoomResponse
    )


updateTask : Json data -> Config -> Model data -> Task Error (Maybe (OpenRoom data))
updateTask json config model =
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
                OpenRoom.getResponse config { dataJson = json, roomId = model.roomId }
                    |> Task.mapError HttpError
            )


update :
    { json : Json data, update : msg -> data -> ( data, Cmd msg ), config : Config }
    -> Msg data msg
    -> Model data
    -> Action data msg
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
            Action.updating
                ( { model
                    | message = Just <| "Synchronizing..."
                    , lastUpdated = lastUpdated
                  }
                , updateTask input.json input.config model
                    |> Task.attempt GotRoomResponse
                )

        GotRoomResponse result ->
            case result of
                Ok (Just room) ->
                    case room.game of
                        Just game ->
                            Action.updating
                                ( initialModel input.config
                                    { room = room
                                    , playerId = model.playerId
                                    , hosting = model.hosting
                                    , lastUpdated = model.lastUpdated
                                    , seed = model.seed
                                    , game = game
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

        GameSpecific gameMsg ->
            let
                ( newGameData, cmd ) =
                    input.update gameMsg model.game.data

                game =
                    model.game
            in
            Action.updating
                ( { model | game = { game | data = newGameData } }
                , cmd |> Cmd.map GameSpecific
                )


subscriptions : (data -> Sub msg) -> Model data -> Sub (Msg data msg)
subscriptions fun model =
    Sub.batch
        [ fun model.game.data |> Sub.map GameSpecific
        , Time.every (1000 * 5) TimePassed
        ]


view :
    { dataView : data -> Element msg2
    , msgMapper : Msg data msg1 -> msg2
    }
    -> Model data
    ->
        { element : Element msg2
        , message : Maybe String
        , error : Maybe Http.Error
        }
view { dataView, msgMapper } { game, hosting, message, error, playerId } =
    { element =
        Element.column (Grid.simple ++ Card.fill) <|
            [ Element.map msgMapper <|
                Element.row Grid.spacedEvenly <|
                    [ Element.el Heading.h1 <|
                        Element.text <|
                            if game.currentPlayer == playerId then
                                "Your turn"

                            else
                                "waiting..."
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
            , dataView game.data
            ]
    , message = message
    , error = error
    }
