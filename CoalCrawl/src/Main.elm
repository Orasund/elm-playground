module Main exposing (..)

import Browser exposing (Document)
import Config
import Data.Block exposing (Block(..))
import Data.Game exposing (Game)
import Data.Game.Behavior
import Data.Info
import Dict exposing (Dict)
import Html
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
    , seed : Seed
    }


updateGame : (Game -> Generator Game) -> Model -> Model
updateGame fun model =
    Random.step (fun model.game) model.seed
        |> (\( game, seed ) ->
                { model
                    | game = game
                    , seed = seed
                }
           )


type Msg
    = Restart Seed
    | TileClicked ( Int, Int )
    | TimePassed
    | TogglePause
    | BuildWagon


restart : Seed -> Model
restart seed =
    Data.Game.new
        |> (\game ->
                { game = game
                , paused = False
                , camera = game.player.pos
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
        [ model.game |> View.Screen.fromGame { onPress = TileClicked, camera = model.camera }
        , (if model.paused then
            "Unpause"

           else
            "Pause"
          )
            |> View.Button.toHtml TogglePause
        , model.game.world
            |> Dict.get model.game.selected
            |> Maybe.map
                (\block ->
                    [ block
                        |> Data.Info.fromBlock model.game
                        |> View.Info.toHtml
                    , case block of
                        Data.Block.Ground Nothing ->
                            "Build Wagon" |> View.Button.toHtml BuildWagon

                        _ ->
                            Layout.none
                    ]
                        |> Layout.column []
                )
            |> Maybe.withDefault Layout.none
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
                |> updateGame Data.Game.Behavior.passTime
            , Cmd.none
            )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        BuildWagon ->
            ( { model | game = model.game |> Data.Game.buildWagon }
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
