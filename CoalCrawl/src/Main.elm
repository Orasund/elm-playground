port module Main exposing (..)

import AnyBag
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Config
import Data.Actor exposing (Actor)
import Data.Behavior
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity(..))
import Data.Game exposing (Game)
import Data.Improvement exposing (Improvement)
import Data.Info
import Data.Item exposing (Item)
import Data.Modal exposing (Modal)
import Data.Sound
import Data.Train
import Data.Zoom
import Html
import Html.Attributes as Attr
import Layout
import Random exposing (Generator, Seed)
import Task
import Time
import View.Button
import View.Modal
import View.Screen
import View.Tab exposing (Tab(..))


port loadSound : ( String, String ) -> Cmd msg


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type BuildMode
    = BuildingBlock ( ( Item, Int ), Block )
    | BuildingActor ( ( Item, Int ), Actor )
    | RemovingBlock


type alias Model =
    { game : Game
    , camera : ( Int, Int )
    , modal : Maybe Modal
    , seed : Seed
    , volume : Int
    , sidebarTab : Maybe Tab
    , tickInterval : Float
    , zoomPercent : Int
    , building : Maybe BuildMode
    , level : Int
    , widthOverHeight : Float
    }


updateGame : (Game -> Generator ( Game, List Effect )) -> Model -> ( Model, Cmd msg )
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( ( game, list ), seed ) ->
                list
                    |> List.foldl
                        (\effect ->
                            case effect of
                                Data.Effect.PlaySound sound ->
                                    Tuple.mapSecond ((::) (sound |> Data.Sound.toString |> playSound))

                                Data.Effect.OpenModal modal ->
                                    Tuple.mapFirst (\m -> { m | modal = Just modal })

                                Data.Effect.LevelUp ->
                                    Tuple.mapFirst
                                        (\m ->
                                            { m
                                                | tickInterval = m.tickInterval * 0.9
                                                , level = m.level + 1
                                            }
                                        )
                        )
                        ( { model
                            | game = game
                            , seed = seed
                            , modal = Nothing
                          }
                        , []
                        )
                    |> Tuple.mapSecond Cmd.batch
           )


type Msg
    = Restart Seed
    | TileClicked ( Int, Int )
    | TimePassed
    | StartBuilding BuildMode
    | StopBuilding
    | CloseModal (Maybe Improvement)
    | SetVolume (Maybe Int)
    | SetTab (Maybe Tab)
    | SetZoom (Maybe Int)
    | SetWidthOverHeight Float


restart : Seed -> Model
restart seed =
    Data.Game.new
        |> (\game ->
                { game = game
                , camera = game.player.pos
                , seed = seed
                , modal = Just Data.Modal.title
                , volume = 25
                , sidebarTab = Just DetailTab
                , tickInterval = 200
                , zoomPercent = 25
                , building = Nothing
                , level = 1
                , widthOverHeight = 1.4
                }
           )


init : () -> ( Model, Cmd Msg )
init () =
    ( Random.initialSeed 42
        |> restart
    , Cmd.batch
        [ Random.generate Restart Random.independentSeed
        , Browser.Dom.getViewport
            |> Task.map (\{ viewport } -> viewport.width / viewport.height)
            |> Task.perform SetWidthOverHeight
        ]
    )


view : Model -> Document Msg
view model =
    let
        train =
            model.game |> Data.Game.getTrain
    in
    { title = "⛏Coal Crawl"
    , body =
        [ Html.node "link"
            [ Attr.rel "stylesheet"
            , Attr.href "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
            ]
            []
        , model.game
            |> View.Screen.fromGame
                { onPress = TileClicked
                , camera = model.camera
                , zoom = Data.Zoom.fromPercent model.zoomPercent
                , widthOverHeight = model.widthOverHeight
                }
            |> Layout.withStack
                ((if model.modal /= Nothing then
                    [ Attr.style "backdrop-filter" "brightness(0.5)" ]

                  else
                    []
                 )
                    ++ [ Attr.style "position" "relative"
                       ]
                )
                [ ( [ Attr.style "top" "8px"
                    , Attr.style "left" "8px"
                    ]
                  , case model.building of
                        Just block ->
                            [ "Stop Building"
                                |> View.Button.toHtml (Just StopBuilding)
                            , (case block of
                                BuildingBlock ( _, b ) ->
                                    b
                                        |> Data.Info.fromBlock model.game
                                        |> .title

                                BuildingActor ( _, a ) ->
                                    a
                                        |> Data.Info.fromActor
                                        |> .title

                                RemovingBlock ->
                                    "Removing"
                              )
                                |> Html.text
                                |> Layout.el
                                    [ Attr.style "background-color" "white"
                                    , Attr.style "padding" "8px"
                                    , Attr.style "border-radius" "8px"
                                    , Attr.style "border" "solid 1px black"
                                    ]
                            ]
                                |> Layout.column [ Layout.spacing 8 ]

                        Nothing ->
                            model.game
                                |> View.Tab.sidebar
                                    { restart = Restart model.seed
                                    , destroyBlock = StartBuilding RemovingBlock
                                    , buildActor =
                                        \{ cost, actor } ->
                                            StartBuilding (BuildingActor ( cost, actor ))
                                    , buildBlock =
                                        \{ cost, block } ->
                                            StartBuilding (BuildingBlock ( cost, block ))
                                    , setVolume = SetVolume
                                    , volume = model.volume
                                    , setZoom = SetZoom
                                    , zoom = model.zoomPercent
                                    , setTab = SetTab
                                    , tab = model.sidebarTab
                                    }
                  )
                , ( [ Attr.style "top" "8px"
                    , Attr.style "right" "8px"
                    ]
                  , [ (( "Tracks", train.tracks )
                        :: (train.items
                                |> AnyBag.toAssociationList
                           )
                        |> List.map (\( k, n ) -> String.fromInt n ++ "x " ++ k)
                      )
                        |> (\content -> content |> String.join ", ")
                        |> Html.text
                        |> Layout.el []
                    , "Needs "
                        ++ (Data.Train.coalNeeded train
                                - AnyBag.count Data.Item.Coal train.items
                                |> String.fromInt
                           )
                        ++ " for the next Level"
                        |> Html.text
                        |> Layout.el []
                    ]
                        |> Layout.column
                            [ Attr.style "background-color" "white"
                            , Attr.style "padding" "8px"
                            , Attr.style "border-radius" "8px"
                            , Attr.style "border" "solid 1px black"
                            ]
                  )
                ]
            |> Layout.withStack []
                [ ( [ Attr.style "top" "50%"
                    , Attr.style "left" "50%"
                    , Attr.style "transform" "translate(-50%,-50%)"
                    ]
                  , case model.modal of
                        Just modal ->
                            View.Modal.toHtml model.widthOverHeight CloseModal model.game modal model.level

                        Nothing ->
                            Layout.none
                  )
                ]
            |> Layout.container [ Attr.style "background-color" "black" ]
        ]
    }


updateCamera : Model -> Model
updateCamera model =
    let
        ( pX, pY ) =
            model.game.player.pos

        ( x, y ) =
            model.camera
    in
    if
        (abs (pX - x) + abs (pY - y))
            > Config.maxCameraDistance (Data.Zoom.fromPercent model.zoomPercent)
                (Config.width model.widthOverHeight)
                Config.height
    then
        { model | camera = ( pX, pY ) }

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart seed ->
            ( restart seed, Cmd.none )

        TileClicked pos ->
            model
                |> updateGame
                    (\game ->
                        case model.building of
                            Just buildingMode ->
                                (case buildingMode of
                                    BuildingBlock ( cost, block ) ->
                                        Data.Game.buildBlock pos cost block game

                                    BuildingActor ( cost, actor ) ->
                                        Data.Game.buildActor pos cost actor game

                                    RemovingBlock ->
                                        Data.Game.destroyBlock pos game
                                )
                                    |> Maybe.withDefault
                                        ( model.game
                                        , [ Data.Effect.PlaySound Data.Sound.Error ]
                                        )
                                    |> Random.constant

                            Nothing ->
                                game
                                    |> Data.Game.select pos
                                    |> Data.Effect.withNone
                    )

        TimePassed ->
            case model.modal of
                Just modal ->
                    ( { model | modal = modal |> Data.Modal.timePassed |> Just }, Cmd.none )

                Nothing ->
                    if model.building == Nothing then
                        model
                            |> updateCamera
                            |> updateGame Data.Behavior.passTime

                    else
                        ( model, Cmd.none )

        StartBuilding a ->
            ( { model | building = Just a }, Cmd.none )

        StopBuilding ->
            ( { model | building = Nothing }, Cmd.none )

        CloseModal maybeImprovement ->
            ( { model
                | modal = Nothing
                , game =
                    maybeImprovement
                        |> Maybe.map (Data.Game.addImprovementTo model.game)
                        |> Maybe.withDefault model.game
              }
            , Data.Sound.asList
                |> List.map (\sound -> loadSound ( Data.Sound.toFile sound, Data.Sound.toString sound ))
                |> Cmd.batch
            )

        SetVolume amount ->
            amount
                |> Maybe.map (\int -> ( { model | volume = int }, setVolume (toFloat int / 100) ))
                |> Maybe.withDefault ( model, Cmd.none )

        SetTab tab ->
            ( { model | sidebarTab = tab }, Cmd.none )

        SetZoom amount ->
            amount
                |> Maybe.map (\float -> ( { model | zoomPercent = float }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        SetWidthOverHeight float ->
            ( { model | widthOverHeight = float }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every model.tickInterval (\_ -> TimePassed)
        , Browser.Events.onResize (\w h -> toFloat w / toFloat h |> SetWidthOverHeight)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
