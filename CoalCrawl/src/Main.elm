port module Main exposing (..)

import AnyBag
import Browser exposing (Document)
import Config
import Data.Actor exposing (Actor)
import Data.Animation
import Data.Behavior
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity(..))
import Data.Game exposing (Game)
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
import Time
import View.Button
import View.Modal
import View.Promt
import View.Screen
import View.Tab exposing (Tab(..))


port loadSound : ( String, String ) -> Cmd msg


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type BuildMode
    = BuildingBlock Block
    | BuildingActor Actor


type alias Model =
    { game : Game
    , camera : ( Int, Int )
    , slowedDown : Bool
    , promt : Maybe String
    , modal : Maybe Modal
    , seed : Seed
    , volume : Int
    , sidebarTab : Maybe Tab
    , tickInterval : Float
    , zoomPercent : Int
    , building : Maybe ( ( Item, Int ), BuildMode )
    , level : Int
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

                                Data.Effect.OpenModal ->
                                    Tuple.mapFirst
                                        (\m ->
                                            { m
                                                | modal =
                                                    Data.Animation.animate
                                                        |> Data.Modal.fromAnimation
                                                        |> Just
                                            }
                                        )

                                Data.Effect.LevelUp ->
                                    Tuple.mapFirst
                                        (\m ->
                                            { m
                                                | tickInterval = m.tickInterval * 0.9
                                                , level = m.level + 1
                                            }
                                        )

                                Data.Effect.ShowPromt string ->
                                    Tuple.mapFirst (\m -> { m | promt = Just string })
                        )
                        ( { model
                            | game = game
                            , seed = seed
                            , modal = Nothing
                            , promt = Nothing
                          }
                        , []
                        )
                    |> Tuple.mapSecond Cmd.batch
           )


type Msg
    = Restart Seed
    | TileClicked ( Int, Int )
    | TimePassed
    | ToggleSlowdown
    | StartBuilding ( ( Item, Int ), BuildMode )
    | StopBuilding
    | CloseModal
    | DestroyBlock
    | SetVolume (Maybe Int)
    | SetTab (Maybe Tab)
    | SetZoom (Maybe Int)


restart : Seed -> Model
restart seed =
    Data.Game.new
        |> (\game ->
                { game = game
                , slowedDown = False
                , camera = game.player.pos
                , promt = Nothing
                , seed = seed
                , modal =
                    Data.Animation.tutorial
                        |> Data.Modal.fromAnimation
                        |> Just
                , volume = 50
                , sidebarTab = Just DetailTab
                , tickInterval = 200
                , zoomPercent = 50
                , building = Nothing
                , level = 1
                }
           )


init : () -> ( Model, Cmd Msg )
init () =
    ( Random.initialSeed 42
        |> restart
    , Random.generate Restart Random.independentSeed
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
        , [ model.game
                |> View.Screen.fromGame
                    { onPress = TileClicked
                    , camera = model.camera
                    , zoom = Data.Zoom.fromPercent model.zoomPercent
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
                            Just ( _, block ) ->
                                [ "Stop Building"
                                    |> View.Button.toHtml (Just StopBuilding)
                                , (case block of
                                    BuildingBlock b ->
                                        b
                                            |> Data.Info.fromBlock model.game

                                    BuildingActor a ->
                                        a |> Data.Info.fromActor
                                  )
                                    |> .title
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
                                        { toggleSlowdown = ToggleSlowdown
                                        , restart = Restart model.seed
                                        , destroyBlock = DestroyBlock
                                        , buildActor =
                                            \{ cost, actor } ->
                                                StartBuilding ( cost, BuildingActor actor )
                                        , buildBlock =
                                            \{ cost, block } ->
                                                StartBuilding ( cost, BuildingBlock block )
                                        , slowedDown = model.slowedDown
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
                    , ( [ Attr.style "left" "20%"
                        , Attr.style "bottom" "8px"
                        , Attr.style "width" "60%"
                        ]
                      , model.promt
                            |> View.Promt.fromString
                      )
                    ]
          , case model.modal of
                Just modal ->
                    View.Modal.toHtml CloseModal model.game modal model.level

                Nothing ->
                    Layout.none
          ]
            |> Html.div
                [ Attr.style "position" "relative"
                ]
            |> List.singleton
            |> Layout.row [ Layout.fill, Layout.centerContent ]
            |> Layout.container [ Attr.style "background-color" "white" ]
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
            > Config.maxCameraDistance (Data.Zoom.fromPercent model.zoomPercent) Config.width Config.height
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
                            Just ( cost, buildingMode ) ->
                                game
                                    |> (case buildingMode of
                                            BuildingBlock block ->
                                                Data.Game.buildBlock pos cost block

                                            BuildingActor actor ->
                                                Data.Game.buildActor pos cost actor
                                       )
                                    |> Maybe.map
                                        (\g ->
                                            ( g, [ Data.Effect.PlaySound Data.Sound.Build ] )
                                                |> Random.constant
                                        )
                                    |> Maybe.withDefault (model.game |> Data.Effect.withNone)

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

        ToggleSlowdown ->
            ( { model | slowedDown = not model.slowedDown }, Cmd.none )

        StartBuilding a ->
            ( { model | building = Just a }, Cmd.none )

        StopBuilding ->
            ( { model | building = Nothing }, Cmd.none )

        DestroyBlock ->
            ( { model | game = model.game |> Data.Game.destroyBlock }, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.slowedDown then
        Time.every (model.tickInterval * 2)
            (\_ -> TimePassed)

    else
        Time.every model.tickInterval (\_ -> TimePassed)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
