module Main exposing (..)

import Browser
import Config
import Dict exposing (Dict)
import Game
import Game.Gravity
import Game.Merge
import Game.Type exposing (Game, TileId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Layout
import Random exposing (Generator, Seed)


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
    List.repeat Config.size ()
        |> List.indexedMap
            (\y () ->
                List.repeat Config.size ()
                    |> List.indexedMap
                        (\x () ->
                            model.displayGrid
                                |> Dict.get ( x, y )
                                |> Maybe.andThen
                                    (\id ->
                                        model.game.tiles
                                            |> Dict.get id
                                    )
                                |> Maybe.map String.fromInt
                                |> Maybe.withDefault ""
                                |> (\value ->
                                        Html.text value
                                            |> Layout.button
                                                ([ Html.Attributes.style "aspect-ratio" "1"
                                                 , Html.Attributes.style "width" "100px"
                                                 , Html.Attributes.style "border" "1px solid black"
                                                 ]
                                                    ++ Layout.centered
                                                    ++ (case model.dragging of
                                                            Just { to } ->
                                                                (if to == ( x, y ) then
                                                                    [ Html.Attributes.style "font-weight" "bold" ]

                                                                 else
                                                                    []
                                                                )
                                                                    ++ [ if to == ( x, y ) then
                                                                            StopDragging
                                                                                |> Html.Events.onMouseUp

                                                                         else
                                                                            DragTo ( x, y )
                                                                                |> Html.Events.onMouseEnter
                                                                       ]

                                                            Nothing ->
                                                                []
                                                       )
                                                )
                                                { onPress =
                                                    case model.dragging of
                                                        Just { to } ->
                                                            if to == ( x, y ) then
                                                                StopDragging |> Just

                                                            else
                                                                DragTo ( x, y ) |> Just

                                                        Nothing ->
                                                            StartDraggingFrom ( x, y ) |> Just
                                                , label = value
                                                }
                                   )
                        )
                    |> Layout.row []
            )
        |> Layout.column []


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
