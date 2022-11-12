port module Main exposing (..)

import Browser exposing (Document)
import Config
import Data.Actor exposing (Actor)
import Data.Animation
import Data.Behavior
import Data.Block exposing (Block(..))
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity(..))
import Data.Game exposing (Game)
import Data.Item exposing (Item)
import Data.Modal exposing (Modal)
import Data.Sound
import Html
import Html.Attributes as Attr
import Layout
import Random exposing (Generator, Seed)
import Time
import View.Game
import View.Modal
import View.Promt


port loadSound : ( String, String ) -> Cmd msg


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type alias Model =
    { game : Game
    , camera : ( Int, Int )
    , slowedDown : Bool
    , promt : Maybe String
    , modal : Maybe Modal
    , seed : Seed
    , volume : Int
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
    | BuildBlock { cost : ( Item, Int ), block : Block }
    | BuildActor { cost : ( Item, Int ), actor : Actor }
    | CloseModal
    | DestroyBlock
    | SetVolume String


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
                    Data.Animation.animate
                        |> Data.Modal.fromAnimation
                        |> Just
                , volume = 100
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
    { title = "Coal Crawl"
    , body =
        [ Html.node "link"
            [ Attr.rel "stylesheet"
            , Attr.href "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
            ]
            []
        , [ model.promt
                |> View.Promt.fromString
          , [ model.game
                |> View.Game.toHtml
                    { tileClicked = TileClicked
                    , toggleSlowdown = ToggleSlowdown
                    , restart = Restart model.seed
                    , destroyBlock = DestroyBlock
                    , buildActor = BuildActor
                    , buildBlock = BuildBlock
                    , camera = model.camera
                    , slowedDown = model.slowedDown
                    , setVolume = SetVolume
                    , volume = model.volume
                    }
                |> Layout.row
                    (if model.modal /= Nothing then
                        [ Attr.style "backdrop-filter" "brightness(0.5)" ]

                     else
                        []
                    )
            , case model.modal of
                Just modal ->
                    View.Modal.toHtml CloseModal model.game modal

                Nothing ->
                    Layout.none
            ]
                |> Html.div [ Attr.style "position" "relative" ]
          ]
            |> Layout.column [ Layout.spacing 8 ]
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
    if abs (pX - x) + abs (pY - y) > Config.maxCameraDistance then
        { model | camera = ( pX, pY ) }

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart seed ->
            ( restart seed, Cmd.none )

        TileClicked pos ->
            ( { model | game = model.game |> Data.Game.select pos }
            , Cmd.none
            )

        TimePassed ->
            case model.modal of
                Just modal ->
                    ( { model | modal = modal |> Data.Modal.timePassed |> Just }, Cmd.none )

                Nothing ->
                    model
                        |> updateCamera
                        |> updateGame Data.Behavior.passTime

        ToggleSlowdown ->
            ( { model | slowedDown = not model.slowedDown }, Cmd.none )

        BuildBlock { cost, block } ->
            ( { model | game = model.game |> Data.Game.buildBlock cost block }
            , Cmd.none
            )

        BuildActor { cost, actor } ->
            ( { model | game = model.game |> Data.Game.buildActor cost actor }, Cmd.none )

        DestroyBlock ->
            ( { model | game = model.game |> Data.Game.destroyBlock }, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }
            , Data.Sound.asList
                |> List.map (\sound -> loadSound ( Data.Sound.toFile sound, Data.Sound.toString sound ))
                |> Cmd.batch
            )

        SetVolume amount ->
            String.toInt amount
                |> Maybe.map (\int -> ( { model | volume = int }, setVolume (toFloat int / 100) ))
                |> Maybe.withDefault ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.slowedDown then
        Time.every 400 (\_ -> TimePassed)

    else
        Time.every 200 (\_ -> TimePassed)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
