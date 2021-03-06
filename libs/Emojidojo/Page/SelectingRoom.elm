module Emojidojo.Page.SelectingRoom exposing (Model, Msg, TransitionData, init, subscriptions, update, view)

import Action
import Dict
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Emojidojo.Data as Data
import Emojidojo.Data.Config exposing (Config)
import Emojidojo.Data.Game as Game exposing (Game)
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
import Http
import Jsonstore exposing (Json)
import Random exposing (Seed)
import String
import Task exposing (Task)
import Time exposing (Posix)


type alias TransitionData remote =
    { openRooms : List OpenRoom
    , games : List (Game remote)
    , lastUpdated : Posix
    , seed : Seed
    }


type alias Model remote =
    { activeRooms : List OpenRoom
    , oldRooms : List OpenRoom
    , activeGames : List (Game remote)
    , oldGames : List (Game remote)
    , lastUpdated : Posix
    , error : Maybe Http.Error
    , message : Maybe String
    , seed : Seed
    , playerId : Id
    }


type Msg remote
    = HostRoom
    | GotOpenRoomResponse (Result Error ( List OpenRoom, List (Game remote) ))
    | CreatedRoom OpenRoom
    | JoinedRoom OpenRoom
    | TimePassed Posix


type Error
    = HttpError Http.Error
    | WrongVersion Float


type alias Action remote =
    Action.Action (Model remote) (Msg remote) InRoom.TransitionData ()


initialModel : Config -> TransitionData remote -> Model remote
initialModel config ({ lastUpdated } as data) =
    let
        ( activeRooms, oldRooms ) =
            data.openRooms
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

        ( activeGames, oldGames ) =
            data.games
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

        ( playerId, seed ) =
            data.seed
                |> Random.step Id.generate
    in
    { activeRooms = activeRooms
    , oldRooms = oldRooms
    , activeGames = activeGames
    , oldGames = oldGames
    , lastUpdated = lastUpdated
    , error = Nothing
    , message = Nothing
    , seed = seed
    , playerId = playerId
    }


init : Json remote -> Config -> TransitionData remote -> ( Model remote, Cmd (Msg remote) )
init jsonData config data =
    let
        model : Model remote
        model =
            initialModel config data
    in
    ( model
    , updateTask jsonData config model
        |> Task.attempt GotOpenRoomResponse
    )


updateTask : Json remote -> Config -> Model remote -> Task Error ( List OpenRoom, List (Game remote) )
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
                            Task.fail (WrongVersion float)

                    Nothing ->
                        Version.insertResponse config
                            |> Task.mapError HttpError
            )
        |> Task.andThen
            (\() ->
                case model.oldRooms |> List.head of
                    Just { id } ->
                        OpenRoom.removeResponse config id
                            |> Task.mapError HttpError

                    Nothing ->
                        Task.succeed ()
            )
        |> Task.andThen
            (\() ->
                case model.oldGames |> List.head of
                    Just { id } ->
                        Game.removeResponse config id
                            |> Task.mapError HttpError

                    Nothing ->
                        Task.succeed ()
            )
        |> Task.andThen
            (\() -> OpenRoom.getListResponse config |> Task.mapError HttpError)
        |> Task.andThen
            (\maybeRoomList ->
                Game.getListResponse config jsonData
                    |> Task.mapError HttpError
                    |> Task.map (Tuple.pair maybeRoomList)
            )


update : Json remote -> Config -> Msg remote -> Model remote -> Action remote
update jsonData config msg model =
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
                , OpenRoom.insertResponse config openRoom
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
                Ok ( maybeRoomList, maybeGameList ) ->
                    Action.updating <|
                        ( initialModel config
                            { openRooms = maybeRoomList
                            , games = maybeGameList
                            , lastUpdated = model.lastUpdated
                            , seed = model.seed
                            }
                        , Cmd.none
                        )

                Err error ->
                    case error of
                        HttpError err ->
                            Action.updating ( { model | error = Just err }, Cmd.none )

                        WrongVersion float ->
                            Action.updating <|
                                if float > config.version then
                                    ( { model
                                        | message =
                                            Just <|
                                                "You are running version "
                                                    ++ String.fromFloat config.version
                                                    ++ ". The current version is "
                                                    ++ String.fromFloat float
                                                    ++ ". Please refresh the page in order to upgrade to the new version."
                                      }
                                    , Cmd.none
                                    )

                                else
                                    ( { model | message = Just "updating..." }
                                    , Jsonstore.delete (Data.url config)
                                        |> Task.mapError HttpError
                                        |> Task.andThen
                                            (\() -> updateTask jsonData config model)
                                        |> Task.attempt GotOpenRoomResponse
                                    )

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
                , updateTask jsonData config newModel
                    |> Task.attempt GotOpenRoomResponse
                )


subscriptions : Model remote -> Sub (Msg remote)
subscriptions _ =
    Time.every (1000 * 5) TimePassed


view :
    Model remote
    ->
        { element : Element (Msg remote)
        , message : Maybe String
        , error : Maybe Http.Error
        }
view { activeRooms, oldRooms, activeGames, oldGames, error, message, playerId } =
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
            , activeGames
                |> List.map
                    (\{ id } ->
                        Element.el
                            Card.large
                        <|
                            Element.text <|
                                Id.view <|
                                    id
                    )
                |> Element.wrappedRow Grid.simple
            , oldGames
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
