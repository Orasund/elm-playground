module Main exposing (..)

import Browser
import Common
import Dict exposing (Dict)
import Game
import Game.Gravity
import Game.Merge
import Game.Type exposing (Game, TileId)
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Random exposing (Generator, Seed)
import View


type alias Model =
    { game : Game
    , displayGrid : Dict ( Int, Int ) TileId
    , seed : Seed
    , dragging :
        Maybe
            { from : ( Int, Int )
            , to : ( Int, Int )
            }
    }


type Msg
    = StartDraggingFrom ( Int, Int )
    | DragTo ( Int, Int )
    | StopDragging
    | GotSeed Seed


init : () -> ( Model, Cmd Msg )
init () =
    let
        game =
            Game.Type.empty
    in
    ( { game = game
      , displayGrid = game.grid
      , seed = Random.initialSeed 42
      , dragging = Nothing
      }
    , Random.generate GotSeed Random.independentSeed
    )


view : Model -> Html Msg
view model =
    Common.grid
        |> List.map
            (\( x, y ) ->
                model.displayGrid
                    |> Dict.get ( x, y )
                    |> Maybe.map
                        (\id ->
                            model.game.tiles
                                |> Dict.get id
                                |> Tuple.pair (String.fromInt id)
                        )
                    |> Maybe.withDefault ( "empty_" ++ String.fromInt x ++ "_" ++ String.fromInt y, Nothing )
                    |> (\( id, value ) ->
                            View.tile
                                { startDraggingFrom = StartDraggingFrom
                                , dragTo = DragTo
                                , stopDragging = StopDragging
                                , dragging = model.dragging
                                }
                                ( ( x, y ), value )
                                |> Tuple.pair id
                       )
            )
        |> Html.Keyed.node "div" [ Html.Attributes.style "position" "relative" ]


applyModel : Model -> Generator Model -> Model
applyModel { seed } gen =
    Random.step gen seed
        |> (\( m, s ) -> { m | seed = s })


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withNoCmd : a -> ( a, Cmd Msg )
        withNoCmd a =
            ( a, Cmd.none )
    in
    case msg of
        StartDraggingFrom pos ->
            { from = pos, to = pos }
                |> Just
                |> (\dragging -> { model | dragging = dragging })
                |> withNoCmd

        DragTo pos ->
            model.dragging
                |> Maybe.map (\dragging -> { dragging | to = pos })
                |> Maybe.map
                    (\dragging ->
                        { model
                            | dragging = Just dragging
                            , displayGrid =
                                model.game.grid
                                    |> Game.drag dragging
                        }
                    )
                |> Maybe.withDefault model
                |> withNoCmd

        StopDragging ->
            model.game
                |> (\game ->
                        { game | grid = model.displayGrid }
                   )
                |> Game.Merge.mergeTiles
                --|> Game.Gravity.apply
                |> Game.refill
                |> Random.map
                    (\game ->
                        { model
                            | game = game
                            , displayGrid = game.grid
                            , dragging = Nothing
                        }
                    )
                |> applyModel model
                |> withNoCmd

        GotSeed s ->
            let
                ( game, seed ) =
                    Random.step Game.generate s
            in
            ( { model
                | game = game
                , displayGrid = game.grid
                , seed = seed
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
