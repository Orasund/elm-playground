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
    | Build { cost : Int, block : Block }
    | CloseModal


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
    , [ (if model.slowedDown then
            "Stop Slow Motion"

         else
            "Start Slow Motion"
        )
            |> View.Button.toHtml ToggleSlowdown
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
                                            Data.Entity.Wagon Data.Wagon.emptyWagon
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
                                                            |> AnyBag.count Data.Item.Iron
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

        Build { cost, block } ->
            ( { model | game = model.game |> Data.Game.buildBlock cost block }
            , Cmd.none
            )

        CloseModal ->
            ( { model | showModal = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.slowedDown || model.showModal then
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
