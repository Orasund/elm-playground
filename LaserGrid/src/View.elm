module View exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2)
import Config
import Dict exposing (Dict)
import Game exposing (Game(..), SavedLevel)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import RelativePos
import Set
import View.Svg


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs =
    Layout.column
        ([ Html.Attributes.style "padding" "1rem"
         , Html.Attributes.style "border" "1px solid black"
         , Html.Attributes.style "background-color" "white"
         , Layout.gap 8
         ]
            ++ attrs
        )


tile1 : Cell1 -> Html msg
tile1 cell =
    Cell.cell1ToColor Nothing cell
        |> View.Svg.cell1
            { height = Config.cellSize
            , width = Config.cellSize
            }


tile2 : Dict Int SavedLevel -> Cell2 -> Html msg
tile2 g cell =
    case cell of
        ConnectionCell c ->
            g
                |> Dict.get c.sort.moduleId
                |> Maybe.map
                    (\level ->
                        level.grid
                            |> View.Svg.grid
                                { height = Config.cellSize
                                , width = Config.cellSize
                                , active =
                                    c.sendsTo /= Dict.empty
                                }
                            |> Layout.el
                                [ Html.Attributes.style "transform"
                                    ("rotate(" ++ String.fromInt (c.sort.rotation * 90) ++ "deg)")
                                ]
                    )
                |> Maybe.withDefault Layout.none

        _ ->
            cell
                |> Cell.cell1ToColor Nothing
                |> View.Svg.cell1
                    { height = Config.cellSize
                    , width = Config.cellSize
                    }


grid : { levels : Dict Int SavedLevel, onToggle : ( Int, Int ) -> msg } -> Game -> Html msg
grid args g =
    List.range -1 4
        |> List.map
            (\y ->
                List.range -1 4
                    |> List.map
                        (\x ->
                            g
                                |> tile
                                    (Layout.asButton
                                        { onPress = Just (args.onToggle ( x, y ))
                                        , label = "Toggle " ++ String.fromInt x ++ "," ++ String.fromInt y
                                        }
                                    )
                                    { pos = ( x, y )
                                    , levels = args.levels
                                    }
                        )
                    |> Layout.row []
            )
        |> Layout.column []


tile : List (Attribute msg) -> { pos : ( Int, Int ), levels : Dict Int SavedLevel } -> Game -> Html msg
tile attrs args g =
    (case g of
        Level1 game ->
            game.grid
                |> Dict.get args.pos
                |> Maybe.map tile1

        Level2 game ->
            game.grid
                |> Dict.get args.pos
                |> Maybe.map (tile2 args.levels)
    )
        |> Maybe.withDefault Layout.none
        |> Layout.el
            ([ Html.Attributes.style "width" (String.fromInt Config.cellSize ++ "px")
             , Html.Attributes.style "height" (String.fromInt Config.cellSize ++ "px")
             , Html.Attributes.style "font-size" "56px"
             ]
                ++ Layout.centered
                ++ attrs
            )
