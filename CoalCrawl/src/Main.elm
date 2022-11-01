module Main exposing (..)

import AnyBag
import Browser exposing (Document)
import Config
import Data.Behavior
import Data.Block exposing (Block(..))
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Info
import Data.Item
import Data.World
import Dict
import Html
import Html.Attributes as Attr
import Layout
import Random exposing (Generator, Seed)
import Time
import View.Button
import View.Info
import View.Screen


type alias Model =
    { game : Game
    , camera : ( Int, Int )
    , paused : Bool
    , promt : Maybe String
    , seed : Seed
    }


updateGame : (Game -> Generator ( Game, Maybe String )) -> Model -> Model
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( ( game, promt ), seed ) ->
                { model
                    | game = game
                    , seed = seed
                    , promt = promt
                }
           )


type Msg
    = Restart Seed
    | TileClicked ( Int, Int )
    | TimePassed
    | TogglePause
    | Build { cost : Int, block : Block }


restart : Seed -> Model
restart seed =
    Data.Game.new
        |> (\game ->
                { game = game
                , paused = False
                , camera = game.player.pos
                , promt = Nothing
                , seed = seed
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
        [ model.promt
            |> Maybe.map
                (\s ->
                    s
                        |> Html.text
                        |> Layout.el [ Layout.fill, Attr.style "background-color" "yellow" ]
                )
            |> Maybe.withDefault Layout.none
            |> Layout.el [ Layout.alignAtCenter, Attr.style "height" "32px" ]
        , model.game |> View.Screen.fromGame { onPress = TileClicked, camera = model.camera }
        , (if model.paused then
            "Unpause"

           else
            "Pause"
          )
            |> View.Button.toHtml TogglePause
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
                                    [ { block =
                                            Data.Entity.Wagon (AnyBag.empty Data.Item.toString)
                                                |> Data.Block.EntityBlock
                                      , cost = Config.wagonCost
                                      }
                                    , { block = Data.Floor.Track |> Data.Block.FloorBlock
                                      , cost = Config.trackCost
                                      }
                                    ]
                                        |> List.map
                                            (\args ->
                                                [ "Build "
                                                    ++ (Data.Info.fromBlock model.game args.block).title
                                                    |> View.Button.toHtml (Build args)
                                                , "Costs "
                                                    ++ String.fromInt args.cost
                                                    ++ " Iron, you got "
                                                    ++ (model.game.train.items
                                                            |> AnyBag.count Data.Item.IronOre
                                                            |> String.fromInt
                                                       )
                                                    ++ " Iron."
                                                    |> Html.text
                                                ]
                                                    |> Layout.row [ Layout.spacing 8 ]
                                            )

                                _ ->
                                    []
                           )
                        |> Layout.column []
                )
            |> Maybe.withDefault Layout.none
        ]
            |> Layout.column []
            |> List.singleton
            |> Layout.row [ Layout.fill, Layout.centerContent ]
            |> Layout.container []
            |> List.singleton
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

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Build { cost, block } ->
            ( { model | game = model.game |> Data.Game.buildBlock cost block }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

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
