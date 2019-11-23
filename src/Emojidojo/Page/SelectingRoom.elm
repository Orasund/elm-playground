module Emojidojo.Page.SelectingRoom exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Page.InRoom as InRoom
import Emojidojo.String as String
import Emojidojo.View.Error as Error
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading
import Http exposing (Error)
import Jsonstore
import Random exposing (Seed)
import Task
import Time exposing (Posix)


type alias TransitionData =
    { openRooms : List OpenRoom
    , lastUpdated : Posix
    , seed : Seed
    }


type alias Model =
    { activeRooms : List OpenRoom
    , oldRooms : List OpenRoom
    , lastUpdated : Posix
    , error : Maybe Error
    , message : Maybe String
    , seed : Seed
    , playerId : Int
    }


type Msg
    = HostRoom
    | GotOpenRoomResponse (Result Error (List OpenRoom))
    | CreatedRoom OpenRoom
    | JoinedRoom OpenRoom
    | RemovedOldRoom
    | PressedRetryButton
    | TimePassed Posix


type alias Action =
    Action.Action Model Msg InRoom.TransitionData Never


init : TransitionData -> ( Model, Cmd Msg )
init ({ lastUpdated } as data) =
    let
        ( activeRooms, oldRooms ) =
            data.openRooms
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

        ( playerId, seed ) =
            data.seed
                |> Random.step (Random.int 0 Random.maxInt)

        ( message, cmd ) =
            case oldRooms |> List.head of
                Just { id } ->
                    ( Just "removing old Room..."
                    , OpenRoom.removeResponse id
                        |> Task.attempt (always RemovedOldRoom)
                    )

                Nothing ->
                    ( Nothing
                    , Cmd.none
                    )
    in
    ( { activeRooms = activeRooms
      , oldRooms = oldRooms
      , lastUpdated = lastUpdated
      , error = Nothing
      , message = message
      , seed = seed
      , playerId = playerId
      }
    , cmd
    )


update : Msg -> Model -> Action
update msg model =
    case msg of
        HostRoom ->
            let
                ( openRoom, seed ) =
                    model.seed
                        |> Random.step
                            (Random.int 0 Random.maxInt
                                |> Random.map
                                    (\id ->
                                        { id = id
                                        , lastUpdated = model.lastUpdated
                                        , player = Dict.empty
                                        }
                                    )
                            )
            in
            Action.updating
                ( { model | message = Just "Starting to Host...", seed = seed }
                , OpenRoom.insertResponse openRoom
                    |> Task.attempt (always (CreatedRoom openRoom))
                )

        CreatedRoom openRoom ->
            Action.transitioning
                { playerId = model.playerId
                , room = openRoom
                , hosting = True
                , lastUpdated = model.lastUpdated
                }

        JoinedRoom openRoom ->
            Action.transitioning
                { playerId = model.playerId
                , room = openRoom
                , hosting = False
                , lastUpdated = model.lastUpdated
                }

        GotOpenRoomResponse result ->
            case result of
                Ok maybeList ->
                    Action.updating <|
                        init
                            { openRooms = maybeList
                            , lastUpdated = model.lastUpdated
                            , seed = model.seed
                            }

                Err error ->
                    Action.updating
                        ( { model
                            | error = Just error
                          }
                        , Cmd.none
                        )

        RemovedOldRoom ->
            Action.updating <|
                ( { model | message = Nothing }
                , OpenRoom.getListResponse
                    |> Task.attempt GotOpenRoomResponse
                )

        PressedRetryButton ->
            Action.updating
                ( { model | message = Just <| "Retrying..." }
                , Jsonstore.delete (Data.url ++ String.openRoom)
                    |> Task.andThen (always OpenRoom.getListResponse)
                    |> Task.attempt GotOpenRoomResponse
                )

        TimePassed lastUpdated ->
            Action.updating
                ( { model
                    | message = Just <| "Updating List..."
                    , lastUpdated = lastUpdated
                  }
                , OpenRoom.getListResponse
                    |> Task.attempt GotOpenRoomResponse
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 * 5) TimePassed


view :
    Model
    ->
        { element : Element Msg
        , message : Maybe String
        , error : Maybe ( Error, Msg )
        }
view { activeRooms, oldRooms, error, message, playerId } =
    { element =
        Element.column (Grid.simple ++ Card.fill) <|
            [ Element.el Heading.h1 <|
                Element.text "Select a Room"
            , activeRooms
                |> List.map
                    (\({ id, player } as room) ->
                        Input.button
                            (Button.simple
                                ++ Card.large
                                ++ Color.success
                                ++ (List.singleton <| Font.center)
                            )
                            { onPress = Just <| JoinedRoom <| room
                            , label =
                                Element.row Grid.spacedEvenly <|
                                    [ Element.text <|
                                        String.left 4 <|
                                            String.fromInt <|
                                                id
                                    , Element.text <|
                                        (player
                                            |> Dict.size
                                            |> String.fromInt
                                        )
                                            ++ " Player"
                                    ]
                            }
                    )
                |> Element.wrappedRow Grid.simple
            , Element.row Grid.spacedEvenly <|
                [ Input.button (Button.simple ++ Color.primary)
                    { onPress = Just HostRoom
                    , label = Element.text "Host Room"
                    }
                , Element.text <|
                    "Player Id: "
                        ++ (String.left 4 <| String.fromInt <| playerId)
                ]
            , oldRooms
                |> List.map
                    (\{ id } ->
                        Element.el
                            (Card.large
                                ++ Color.disabled
                            )
                        <|
                            Element.text <|
                                String.left 4 <|
                                    String.fromInt <|
                                        id
                    )
                |> Element.wrappedRow Grid.simple
            ]
    , message = message
    , error = error |> Maybe.map (\err -> ( err, PressedRetryButton ))
    }
