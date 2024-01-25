module View.Game exposing (..)

import Config
import Dict
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Layout
import View.Cell
import View.Color


toHtml : Game -> Html msg
toHtml game =
    [ List.range 0 (game.height - 1)
        |> List.map
            (\y ->
                List.range 0 (game.width - 1)
                    |> List.map
                        (\x ->
                            game.cells
                                |> Dict.get ( x, y )
                                |> Maybe.map View.Cell.toHtml
                                |> Maybe.withDefault View.Cell.empty
                        )
                    |> Layout.row []
            )
        |> Layout.column [ Html.Attributes.style "position" "absolute" ]
    , game.cells
        |> Dict.toList
        |> List.filterMap
            (\( ( x, y ), cell ) ->
                cell.item
                    |> Maybe.andThen
                        (\id ->
                            game.items
                                |> Dict.get id
                                |> Maybe.map
                                    (\item ->
                                        ( String.fromInt id
                                        , Layout.text [] (String.fromInt id)
                                            |> View.Cell.circle
                                                [ Html.Attributes.style "background-color"
                                                    (View.Color.toString item.color)
                                                , Html.Attributes.style "width" (String.fromInt (Config.cellSize // 2) ++ "px")
                                                , Html.Attributes.style "height" (String.fromInt (Config.cellSize // 2) ++ "px")
                                                ]
                                            |> Layout.el
                                                ([ Html.Attributes.style "position" "absolute"
                                                 , Html.Attributes.style "top"
                                                    (String.fromInt (Config.cellSize * y) ++ "px")
                                                 , Html.Attributes.style "left"
                                                    (String.fromInt (Config.cellSize * x) ++ "px")
                                                 , Html.Attributes.style "width" (String.fromInt Config.cellSize ++ "px")
                                                 , Html.Attributes.style "height" (String.fromInt Config.cellSize ++ "px")
                                                 , Html.Attributes.style "transition" "top 1s, left 1s"
                                                 ]
                                                    ++ Layout.centered
                                                )
                                        )
                                    )
                        )
            )
        |> List.sortBy Tuple.first
        |> Html.Keyed.node "div" [ Html.Attributes.style "position" "absolute" ]
    ]
        |> Html.div [ Html.Attributes.style "position" "relative" ]
