module Emojidojo.Page.SelectingRoom exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.Id as Id exposing (Id)
import Emojidojo.Data.OpenRoom as OpenRoom exposing (OpenRoom)
import Emojidojo.Data.Version as Version
import Emojidojo.Page.InRoom as InRoom
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
    , playerId : Id
    }


type Msg
    = HostRoom
    | GotOpenRoomResponse (Result Error (List OpenRoom))
    | CreatedRoom OpenRoom
    | JoinedRoom OpenRoom
    | TimePassed Posix
    | GotVersion (Result Error (Maybe Float))


type alias Action =
    Action.Action Model Msg InRoom.TransitionData ()


initialModel : TransitionData -> Model
initialModel ({ lastUpdated } as data) =
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
                |> Random.step Id.generate
    in
    { activeRooms = activeRooms
    , oldRooms = oldRooms
    , lastUpdated = lastUpdated
    , error = Nothing
    , message = Nothing
    , seed = seed
    , playerId = playerId
    }


init : TransitionData -> ( Model, Cmd Msg )
init data =
    let
        model : Model
        model =
            initialModel data
    in
    ( model
    , Version.getResponse
        |> Task.attempt GotVersion
    )


updateTask : Model -> Task Error (List OpenRoom)
updateTask model =
    (case model.oldRooms |> List.head of
        Just { id } ->
            OpenRoom.removeResponse id

        Nothing ->
            Task.succeed ()
    )
        |> Task.andThen (\() -> OpenRoom.getListResponse)


update : Msg -> Model -> Action
update msg model =
    case msg of
        HostRoom ->
            let
                ( openRoom, seed ) =
                    model.seed
                        |> Random.step
                            (Id.generate
                                |> Random.map
                                    (\id ->
                                        { id = id
                                        , lastUpdated = model.lastUpdated
                                        , player = Dict.empty
                                        , gameId = Nothing
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
                , seed = model.seed
                }

        JoinedRoom openRoom ->
            Action.transitioning
                { playerId = model.playerId
                , room = openRoom
                , hosting = False
                , lastUpdated = model.lastUpdated
                , seed = model.seed
                }

        GotOpenRoomResponse result ->
            case result of
                Ok maybeList ->
                    Action.updating <|
                        ( initialModel
                            { openRooms = maybeList
                            , lastUpdated = model.lastUpdated
                            , seed = model.seed
                            }
                        , Cmd.none
                        )

                Err error ->
                    Action.updating
                        ( { model
                            | error = Just error
                          }
                        , Cmd.none
                        )

        GotVersion result ->
            case result of
                Ok maybeFloat ->
                    if maybeFloat == Just Data.version then
                        Action.updating
                            ( { model
                                | message = Just <| "Updating List..."
                              }
                            , model
                                |> updateTask
                                |> Task.attempt GotOpenRoomResponse
                            )

                    else
                        Action.exiting

                Err error ->
                    Action.updating
                        ( { model
                            | error = Just error
                          }
                        , Cmd.none
                        )

        TimePassed lastUpdated ->
            Action.updating
                ( { model
                    | message = Just <| "Checking Version..."
                    , lastUpdated = lastUpdated
                  }
                , Version.getResponse
                    |> Task.attempt GotVersion
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 * 5) TimePassed


view :
    Model
    ->
        { element : Element Msg
        , message : Maybe String
        , error : Maybe Error
        }
view { activeRooms, oldRooms, error, message, playerId } =
    { element =
        Element.column (Grid.simple ++ Card.fill) <|
            [ Element.el Heading.h1 <|
                Element.text "Select a Room"
            , activeRooms
                |> List.map
                    (\({ id, player, gameId } as room) ->
                        if gameId == Nothing then
                            Input.button
                                (Button.simple
                                    ++ Card.large
                                    ++ Color.success
                                    ++ (List.singleton <| Font.center)
                                )
                                { onPress = Just <| JoinedRoom <| room
                                , label =
                                    Element.row Grid.spacedEvenly <|
                                        [ Element.text <| Id.view id
                                        , Element.text <|
                                            (player
                                                |> Dict.size
                                                |> String.fromInt
                                            )
                                                ++ " Player"
                                        ]
                                }

                        else
                            Element.el
                                (Button.simple
                                    ++ Card.large
                                    ++ (List.singleton <| Font.center)
                                )
                            <|
                                Element.row Grid.spacedEvenly <|
                                    [ Element.text <| Id.view id
                                    , Element.text <| "Game is running"
                                    ]
                    )
                |> Element.wrappedRow Grid.simple
            , Element.row Grid.spacedEvenly <|
                [ Input.button (Button.simple ++ Color.primary)
                    { onPress = Just HostRoom
                    , label = Element.text "Host Room"
                    }
                , Element.text <|
                    "Player Id: "
                        ++ (Id.view <| playerId)
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
                                Id.view <|
                                    id
                    )
                |> Element.wrappedRow Grid.simple
            ]
    , message = message
    , error = error
    }
