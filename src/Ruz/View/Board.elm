module Ruz.View.Board exposing (view)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Html.Keyed
import Layout
import Ruz.Config as Config
import Ruz.Data.Figure as Figure exposing (Figure)


view :
    { figures : Dict Int Figure
    , player : ( Int, Int )
    , overlay : Dict ( Int, Int ) Color
    , onClick : ( Int, Int ) -> msg
    }
    -> Dict ( Int, Int ) Int
    -> Html msg
view args board =
    [ args.figures |> viewFigures { board = board }
    , board |> viewBoard args
    ]
        |> Html.div
            [ Attr.style "height" "400px"
            , Attr.style "width" "400px"
            , Attr.style "position" "relative"
            ]


viewFigures : { board : Dict ( Int, Int ) Int } -> Dict Int Figure -> Html msg
viewFigures args figures =
    let
        gridSize =
            100 / toFloat Config.size
    in
    args.board
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.map
            (\( ( x, y ), figureId ) ->
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
                        , Attr.style "font-size" "50px"
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
    , player : ( Int, Int )
    , overlay : Dict ( Int, Int ) Color
    , onClick : ( Int, Int ) -> msg
    }
    -> Dict ( Int, Int ) Int
    -> Html msg
viewBoard args board =
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
