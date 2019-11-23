module Emojidojo.Page.InRoom exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.Player as Player exposing (Player)
import Emojidojo.String as String
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading
import Http exposing (Error)
import Json.Decode as D
import Json.Encode as E
import Jsonstore
import Task exposing (Task)
import Time exposing (Posix)


type alias Model =
    { roomId : Int
    , playerId : Int
    , activePlayers : List Player
    , inactivePlayers : List Player
    , hosting : Bool
    , message : Maybe String
    , error : Maybe Error
    , lastUpdated : Posix
    }


type Msg
    = PressedLeaveRoomButton
    | LeftRoom
    | TimePassed Posix
    | GotRoomResponse (Result Error (Maybe OpenRoom))


type alias TransitionData =
    { room : OpenRoom
    , playerId : Int
    , hosting : Bool
    , lastUpdated : Posix
    }


type alias Action =
    Action.Action Model Msg Never ()


initialModel : TransitionData -> Model
initialModel { room, playerId, hosting, lastUpdated } =
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
    }


init : TransitionData -> ( Model, Cmd Msg )
init data =
    let
        model : Model
        model =
            initialModel data
    in
    ( { model | message = Just "joining room..." }
    , Player.insertResponse model.roomId
        { id = model.playerId
        , lastUpdated = model.lastUpdated
        }
        |> Task.andThen (\() -> updateTask model)
        |> Task.attempt GotRoomResponse
    )


updateTask : Model -> Task Error (Maybe OpenRoom)
updateTask model =
    (if model.hosting then
        OpenRoom.updateResponse
            { roomId = model.roomId
            , lastUpdated = model.lastUpdated
            }
            |> Task.andThen
                (\() ->
                    case model.inactivePlayers of
                        player :: _ ->
                            Player.removeResponse
                                { roomId = model.roomId
                                , playerId = player.id
                                }

                        _ ->
                            Task.succeed ()
                )

     else
        Task.succeed ()
    )
        |> Task.andThen
            (\() ->
                Player.updateResponse
                    { roomId = model.roomId
                    , playerId = model.playerId
                    , lastUpdated = model.lastUpdated
                    }
            )
        |> Task.andThen
            (\() ->
                OpenRoom.getResponse model.roomId
            )


update : Msg -> Model -> Action
update msg model =
    case msg of
        PressedLeaveRoomButton ->
            Action.updating <|
                ( { model | message = Just "Leaving..." }
                , Player.removeResponse { roomId = model.roomId, playerId = model.playerId }
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
            { model | message = Just <| "Synchronizing...", lastUpdated = lastUpdated }
                |> (\m ->
                        ( m
                        , updateTask m
                            |> Task.attempt GotRoomResponse
                        )
                   )
                |> Action.updating

        GotRoomResponse result ->
            case result of
                Ok (Just room) ->
                    Action.updating
                        ( initialModel
                            { room = room
                            , playerId = model.playerId
                            , hosting = model.hosting
                            , lastUpdated = model.lastUpdated
                            }
                        , Cmd.none
                        )

                Ok Nothing ->
                    Action.exiting

                Err error ->
                    Action.updating
                        ( { model
                            | error = Just error
                          }
                        , Cmd.none
                        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 * 5) TimePassed


view :
    Model
    ->
        { element : Element Msg
        , message : Maybe String
        , error : Maybe ( Error, Msg )
        }
view { roomId, playerId, activePlayers, inactivePlayers, hosting, message, error } =
    { element =
        Element.column (Grid.simple ++ Card.fill) <|
            [ Element.el Heading.h1 <|
                Element.text <|
                    "Room Id: "
                        ++ (String.left 4 <|
                                String.fromInt <|
                                    roomId
                           )
            , activePlayers
                |> List.map
                    (\{ id } ->
                        Element.el Card.large <|
                            Element.text <|
                                String.left 4 <|
                                    String.fromInt <|
                                        id
                    )
                |> Element.wrappedRow Grid.simple
            , inactivePlayers
                |> List.map
                    (\{ id } ->
                        Element.el (Card.large ++ Color.disabled) <|
                            Element.text <|
                                String.left 4 <|
                                    String.fromInt <|
                                        id
                    )
                |> Element.wrappedRow Grid.simple
            , Element.row Grid.spacedEvenly <|
                [ Input.button (Button.simple ++ Color.danger)
                    { onPress = Just PressedLeaveRoomButton
                    , label =
                        Element.text <|
                            if hosting then
                                "close Room"

                            else
                                "leave Room"
                    }
                , Element.text <|
                    "Player Id: "
                        ++ (String.left 4 <| String.fromInt <| playerId)
                ]
            ]
    , message = message
    , error = error |> Maybe.map (\err -> ( err, PressedLeaveRoomButton ))
    }
