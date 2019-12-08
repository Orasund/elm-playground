module Emojidojo.Page.InGame exposing (Model, Msg(..), TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Game as Game exposing (Game)
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.Player as Player exposing (Player)
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
    { gameId : Id
    , playerId : Id
    , game : Game data
    , activePlayers : List Player
    , inactivePlayers : List Player
    , hosting : Bool
    , message : Maybe String
    , error : Maybe Http.Error
    , lastUpdated : Posix
    , seed : Seed
    }


type Msg remote msg
    = PressedLeaveRoomButton
    | LeftRoom
    | TimePassed Posix
    | GotGameResponse (Result Error (Maybe (Game remote)))
    | GameSpecific msg
    | PressedEndTurnButton


type alias TransitionData data =
    { game : Game data
    , playerId : Id
    , hosting : Bool
    , lastUpdated : Posix
    , seed : Seed
    }


type Error
    = HttpError Http.Error
    | WrongVersion


type alias Action data remote msg =
    Action.Action (Model data) (Msg remote msg) Never ()


initialModel : Config -> TransitionData data -> Model data
initialModel config { game, playerId, hosting, lastUpdated, seed } =
    let
        ( activePlayers, inactivePlayers ) =
            game.player
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
    { gameId = game.id
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


init : Json remote -> Config -> TransitionData data -> ( Model data, Cmd (Msg remote msg) )
init json config data =
    let
        model : Model data
        model =
            initialModel config data
    in
    ( { model | message = Just "joining room..." }
    , Player.insertResponse config
        model.gameId
        { id = model.playerId
        , lastUpdated = model.lastUpdated
        }
        |> Task.mapError HttpError
        |> Task.andThen (\() -> updateTask json config model)
        |> Task.attempt GotGameResponse
    )


updateTask : Json remote -> Config -> Model data -> Task Error (Maybe (Game remote))
updateTask jsonData config model =
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
                    Game.updateLastUpdatedResponse config
                        { gameId = model.gameId
                        , lastUpdated = model.lastUpdated
                        }
                        |> Task.andThen
                            (\() ->
                                case model.inactivePlayers of
                                    player :: _ ->
                                        Player.removeResponse config
                                            { gameId = model.gameId
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
                Player.updateResponse config
                    { gameId = model.gameId
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
                    Game.removeResponse config model.gameId
                        |> Task.mapError HttpError

                else
                    Task.succeed ()
            )
        |> Task.andThen
            (\() ->
                Game.getResponse config { gameId = model.gameId, jsonData = jsonData }
                    |> Task.mapError HttpError
            )


update :
    { json : Json remote
    , remoteWrapper : remote -> msg
    , update : msg -> data -> ( data, Cmd msg )
    , config : Config
    , remoteFromModel : data -> remote
    , init : remote -> data
    }
    -> Msg remote msg
    -> Model data
    -> Action data remote msg
update input msg model =
    case msg of
        PressedLeaveRoomButton ->
            Action.updating <|
                ( { model | message = Just "Leaving..." }
                , Player.removeResponse input.config
                    { gameId = model.gameId
                    , playerId = model.playerId
                    }
                    |> (if model.hosting then
                            Task.andThen
                                (always
                                    (Game.removeResponse input.config model.gameId)
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
                , updateTask input.json input.config newModel
                    |> Task.attempt GotGameResponse
                )

        GotGameResponse result ->
            case result of
                Ok (Just game) ->
                    let
                        ( data, cmd ) =
                            model.game.data
                                |> input.update (input.remoteWrapper game.data)
                    in
                    Action.updating
                        ( initialModel input.config
                            { playerId = model.playerId
                            , hosting = model.hosting
                            , lastUpdated = model.lastUpdated
                            , seed = model.seed
                            , game = game |> Game.map data
                            }
                        , cmd |> Cmd.map GameSpecific
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

        PressedEndTurnButton ->
            let
                remote : remote
                remote =
                    model.game.data |> input.remoteFromModel
            in
            Action.updating
                ( { game = model.game |> Game.map (remote |> input.init)
                  , playerId = model.playerId
                  , hosting = model.hosting
                  , lastUpdated = model.lastUpdated
                  , seed = model.seed
                  }
                    |> initialModel input.config
                , Game.updateResponse input.config
                    { gameId = model.gameId
                    , jsonData = input.json
                    , remote = remote
                    }
                    |> Task.mapError HttpError
                    |> Task.andThen
                        (\() ->
                            updateTask input.json input.config model
                        )
                    |> Task.attempt GotGameResponse
                )


subscriptions : (data -> Sub msg) -> Model data -> Sub (Msg remote msg)
subscriptions fun model =
    Sub.batch
        [ fun model.game.data |> Sub.map GameSpecific
        , Time.every (1000 * 5) TimePassed
        ]


view :
    { dataView : data -> Element msg2
    , msgMapper : Msg remote msg1 -> msg2
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
            , Element.map msgMapper <|
                if game.currentPlayer == playerId then
                    Input.button (Button.simple ++ Color.success)
                        { onPress = Just PressedEndTurnButton
                        , label = Element.text <| "End Turn"
                        }

                else
                    Element.none
            ]
    , message = message
    , error = error
    }
