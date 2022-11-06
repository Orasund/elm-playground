module Main exposing (..)

import AnyBag
import Browser exposing (Document)
import Config
import Data.Actor exposing (Actor)
import Data.Behavior
import Data.Block exposing (Block(..))
import Data.Entity exposing (Entity(..))
import Data.Floor
import Data.Game exposing (Game)
import Data.Info
import Data.Item
import Data.Wagon
import Data.World
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import Random exposing (Generator, Seed)
import Time
import View.Button
import View.Info
import View.Modal
import View.Promt
import View.Screen


type alias Model =
    { game : Game
    , camera : ( Int, Int )
    , slowedDown : Bool
    , promt : Maybe String
    , showModal : Bool
    , seed : Seed
    }


updateGame : (Game -> Generator ( Game, { promt : Maybe String, showModal : Bool } )) -> Model -> Model
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( ( game, { promt, showModal } ), seed ) ->
                { model
                    | game = game
                    , seed = seed
                    , promt = promt
                    , showModal = showModal
                }
           )


type Msg
    = Restart Seed
    | TileClicked ( Int, Int )
    | TimePassed
    | ToggleSlowdown
    | BuildBlock { cost : Int, block : Block }
    | BuildActor { cost : Int, actor : Actor }
    | CloseModal
    | DestroyBlock


restart : Seed -> Model
restart seed =
    Data.Game.new
        |> (\game ->
                { game = game
                , slowedDown = False
                , camera = game.player.pos
                , promt = Nothing
                , seed = seed
                , showModal = False
                }
           )


init : () -> ( Model, Cmd Msg )
init () =
    ( Random.initialSeed 42
        |> restart
    , Random.generate Restart Random.independentSeed
    )


viewGame : Model -> Html Msg
viewGame model =
    [ model.game
        |> View.Screen.fromGame { onPress = TileClicked, camera = model.camera }
    , [ [ (if model.slowedDown then
            "Stop Slow Motion"

           else
            "Start Slow Motion"
          )
            |> View.Button.toHtml ToggleSlowdown
        , View.Button.toHtml (Restart model.seed) "Restarts"
        ]
            |> Layout.row [ Layout.spacing 8 ]
      , Data.Info.fromTrain model.game |> View.Info.justContent
      , model.game.world
            |> Data.World.get model.game.selected
            |> Maybe.map
                (\block ->
                    (block
                        |> Data.Info.fromBlock model.game
                        |> View.Info.toHtml
                    )
                        :: (case block of
                                Data.Block.FloorBlock (Data.Floor.Ground Nothing) ->
                                    [ { actor = Data.Actor.Wagon Data.Wagon.emptyWagon
                                      , cost = Config.wagonCost
                                      }
                                        |> (\args ->
                                                { cost = args.cost
                                                , button =
                                                    "Build "
                                                        ++ (Data.Info.fromActor args.actor).title
                                                        |> View.Button.toHtml (BuildActor args)
                                                }
                                           )
                                    , { block = Data.Floor.Track |> Data.Block.FloorBlock
                                      , cost = Config.trackCost
                                      }
                                        |> (\args ->
                                                { cost = args.cost
                                                , button =
                                                    "Build "
                                                        ++ (Data.Info.fromBlock model.game args.block).title
                                                        |> View.Button.toHtml (BuildBlock args)
                                                }
                                           )
                                    ]
                                        |> List.map
                                            (\args ->
                                                [ args.button
                                                , "Costs "
                                                    ++ String.fromInt args.cost
                                                    ++ " Iron, you got "
                                                    ++ (model.game.train.items
                                                            |> AnyBag.count Data.Item.Iron
                                                            |> String.fromInt
                                                       )
                                                    ++ " Iron."
                                                    |> Html.text
                                                ]
                                                    |> Layout.row [ Layout.spacing 8 ]
                                            )

                                Data.Block.FloorBlock Data.Floor.Track ->
                                    "Destroy"
                                        |> View.Button.toHtml DestroyBlock
                                        |> List.singleton

                                _ ->
                                    []
                           )
                        |> Layout.column []
                )
            |> Maybe.withDefault Layout.none
      ]
        |> Layout.column [ Layout.spacing 8, Attr.style "width" "300px" ]
    ]
        |> Layout.row
            (if model.showModal then
                [ Attr.style "backdrop-filter" "brightness(0.5)" ]

             else
                []
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
          , [ viewGame model
            , if model.showModal then
                View.Modal.toHtml CloseModal model.game

              else
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
            ( model
                |> updateCamera
                |> updateGame Data.Behavior.passTime
            , Cmd.none
            )

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
            ( { model | showModal = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.slowedDown || model.showModal then
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
