module Ruz.View.Board exposing (view)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed
import Layout
import Ruz.Config as Config
import Ruz.Data.Figure as Figure exposing (Figure, FigureId)


view :
    { figures : Dict FigureId Figure
    , overlay : Dict ( Int, Int ) Color
    , onClick : ( Int, Int ) -> msg
    }
    -> Dict FigureId ( Int, Int )
    -> Html msg
view args figurePos =
    let
        gridSize =
            Config.boardSize / toFloat Config.size
    in
    [ args.figures |> viewFigures { figurePos = figurePos }
    , figurePos |> viewBoard args
    ]
        |> Html.div
            [ Attr.style "height" (String.fromFloat Config.boardSize ++ "px")
            , Attr.style "width" (String.fromFloat Config.boardSize ++ "px")
            , Attr.style "position" "relative"
            ]


viewFigures : { figurePos : Dict FigureId ( Int, Int ) } -> Dict FigureId Figure -> Html msg
viewFigures args figures =
    let
        gridSize =
            100 / toFloat Config.size
    in
    args.figurePos
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map
            (\( figureId, ( x, y ) ) ->
                ( "figure-" ++ String.fromInt figureId
                , [ figures
                        |> Dict.get figureId
                        |> Maybe.map (Figure.toString False)
                        |> Maybe.withDefault (Figure.toString True Figure.player)
                        |> Html.text
                  ]
                    |> Html.div
                        [ Attr.style "position" "absolute"
                        , Attr.style "left" (String.fromFloat (toFloat x * gridSize + gridSize / 4) ++ "%")
                        , Attr.style "top" (String.fromFloat (toFloat y * gridSize + gridSize / 4) ++ "%")
                        , Attr.style "width" (String.fromFloat (gridSize / 2) ++ "%")
                        , Attr.style "height" (String.fromFloat (gridSize / 2) ++ "%")
                        , Attr.style "display" "flex"
                        , Attr.style "justify-content" "center"
                        , Attr.style "align-items" "center"
                        , Attr.style "transition" "top 0.5s, left 0.5s"
                        , Attr.style "font-size" "40px"
                        ]
                )
            )
        |> Html.Keyed.node "div"
            [ Attr.style "position" "absolute"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            ]


viewBoard :
    { figures : Dict Int Figure
    , overlay : Dict ( Int, Int ) Color
    , onClick : ( Int, Int ) -> msg
    }
    -> Dict FigureId ( Int, Int )
    -> Html msg
viewBoard args figurePos =
    List.repeat Config.size ()
        |> List.indexedMap
            (\j () ->
                List.repeat Config.size ()
                    |> List.indexedMap
                        (\i () ->
                            Html.button
                                [ Event.onClick (args.onClick ( i, j ))
                                , Attr.style "border" "solid 1 rgba(0,0,0,25)"
                                , Attr.style "flex" "1"
                                , Attr.style "height" "100%"
                                , Attr.style "background-color"
                                    (args.overlay
                                        |> Dict.get ( i, j )
                                        |> Maybe.withDefault (Color.rgba 0 0 0 0)
                                        |> Color.toCssString
                                    )
                                ]
                                []
                        )
                    |> Layout.row
                        [ Attr.style "flex" "1"
                        ]
            )
        |> Layout.column
            [ Attr.style "height" "100%"
            , Attr.style "width" "100%"
            , Attr.style "position" "absolute"
            ]
