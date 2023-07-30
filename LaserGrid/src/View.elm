module View exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2)
import Color
import Config
import Dict exposing (Dict)
import Game exposing (Game(..), SavedLevel)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import RelativePos
import Set
import View.Svg exposing (RenderFunction)


gameWon : Html msg
gameWon =
    "You Win!"
        |> Layout.text []
        |> Layout.el
            ([ Html.Attributes.style "background" "linear-gradient(45deg,orange, yellow)"
             , Html.Attributes.style "width" ((Config.cellSize * 6 |> String.fromInt) ++ "px")
             , Html.Attributes.style "height" ((Config.cellSize * 6 |> String.fromInt) ++ "px")
             , Html.Attributes.style "color" "white"
             , Html.Attributes.style "font-size" "2rem"
             ]
                ++ Layout.centered
            )


savedLevels : (Int -> msg) -> Dict Int SavedLevel -> Html msg
savedLevels fun dict =
    dict
        |> Dict.toList
        |> List.map
            (\( id, level ) ->
                [ level.grid
                    |> View.Svg.grid
                        { width = Config.cellSize
                        , height = Config.cellSize
                        , active = \_ -> False
                        , laserColor = Color.laserColorLevel1
                        , render = \_ -> View.Svg.boxRender
                        }
                , Layout.text [] ("Stage" ++ String.fromInt id)
                ]
                    |> Layout.column
                        (Layout.asButton
                            { label = "Select Level"
                            , onPress = fun id |> Just
                            }
                            ++ [ Layout.gap 8
                               , Layout.alignAtCenter
                               ]
                        )
            )
        |> Layout.row [ Layout.gap 16 ]


title : String -> Html msg
title =
    Layout.text [ Html.Attributes.style "font-size" "2rem" ]


cardTitle : String -> Html msg
cardTitle =
    Layout.text [ Html.Attributes.style "font-size" "1.5rem" ]


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs =
    Layout.column
        ([ Html.Attributes.style "padding" "2rem"
         , Html.Attributes.style "background-color" Color.lightGray
         , Html.Attributes.style "border-radius" "1rem"
         , Layout.gap 16
         ]
            ++ attrs
        )


tile1 : Cell1 -> Html msg
tile1 cell =
    Cell.cell1ToColor { laserColor = Color.laserColorLevel1 }
        Nothing
        cell
        |> View.Svg.cell1
            { height = Config.cellSize
            , width = Config.cellSize
            , render = cellRender cell
            }


tile2 : Dict Int SavedLevel -> Cell2 -> Html msg
tile2 g cell =
    case cell of
        ConnectionCell c ->
            g
                |> Dict.get c.moduleId
                |> Maybe.map
                    (\level ->
                        let
                            activePos =
                                c.sendsTo
                                    |> Dict.keys
                                    |> List.map (RelativePos.rotate (4 - c.rotation))
                                    |> List.concatMap
                                        (\to ->
                                            level.connections
                                                |> Dict.get to
                                                |> Maybe.map .path
                                                |> Maybe.withDefault []
                                        )
                                    |> Set.fromList
                        in
                        level.grid
                            |> View.Svg.grid
                                { height = Config.cellSize
                                , width = Config.cellSize
                                , active = \pos -> Set.member (RelativePos.fromTuple pos) activePos
                                , laserColor = Color.laserColorLevel2
                                , render = \_ -> View.Svg.boxRender
                                }
                            |> Layout.el
                                [ Html.Attributes.style "transform"
                                    ("rotate(" ++ String.fromInt (c.rotation * 90) ++ "deg)")
                                ]
                    )
                |> Maybe.withDefault Layout.none

        _ ->
            cell
                |> Cell.cell1ToColor
                    { laserColor = Color.laserColorLevel2 }
                    Nothing
                |> View.Svg.cell1
                    { height = Config.cellSize
                    , width = Config.cellSize
                    , render = cellRender cell
                    }


grid : List (Attribute msg) -> { levels : Dict Int SavedLevel, onToggle : ( Int, Int ) -> msg } -> Game -> Html msg
grid attrs args g =
    List.range -1 4
        |> List.map
            (\y ->
                List.range -1 4
                    |> List.map
                        (\x ->
                            g
                                |> game
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
        |> Layout.column attrs


game : List (Attribute msg) -> { pos : ( Int, Int ), levels : Dict Int SavedLevel } -> Game -> Html msg
game attrs args g =
    (case g of
        Level1 stage ->
            stage.grid
                |> Dict.get args.pos
                |> Maybe.map tile1

        Level2 stage ->
            stage.grid
                |> Dict.get args.pos
                |> Maybe.map (tile2 args.levels)
    )
        |> Maybe.withDefault Layout.none
        |> Layout.el
            ([ Html.Attributes.style "width" (String.fromInt Config.cellSize ++ "px")
             , Html.Attributes.style "height" (String.fromInt Config.cellSize ++ "px")
             ]
                ++ Layout.centered
                ++ attrs
            )


primaryButton : msg -> String -> Html msg
primaryButton onPress label =
    Layout.textButton
        [ Html.Attributes.style "background" Color.fontColor
        , Html.Attributes.style "border" ("1px solid " ++ Color.fontColor)
        , Html.Attributes.style "color" Color.white
        , Html.Attributes.style "padding" "0.5rem"
        , Html.Attributes.style "border-radius" "0.5rem"
        , Html.Attributes.style "font-weight" "bold"
        ]
        { onPress = Just onPress
        , label = label
        }


cellRender : Cell connection -> RenderFunction msg
cellRender cell =
    case cell of
        ConnectionCell _ ->
            View.Svg.boxRender

        Wall ->
            View.Svg.boxRender

        Origin ->
            View.Svg.boxRender

        Target Nothing ->
            View.Svg.targetRender { secondaryColor = Color.laserColorLevel1 }

        Target (Just _) ->
            View.Svg.targetRender { secondaryColor = Color.wallColor }


button : msg -> String -> Html msg
button onPress label =
    Layout.textButton
        [ Html.Attributes.style "background" "transparent"
        , Html.Attributes.style "border" ("1px solid " ++ Color.fontColor)
        , Html.Attributes.style "color" Color.fontColor
        , Html.Attributes.style "padding" "0.5rem"
        , Html.Attributes.style "border-radius" "0.5rem"
        , Html.Attributes.style "font-weight" "bold"
        ]
        { onPress = Just onPress
        , label = label
        }


stylesheet : Html msg
stylesheet =
    """
html,body {
    height: 100%;
    margin:0;
    padding:0;
}
button:hover {
    filter: brightness(1.5);
}
button:focus {
    filter: brightness(2);
}
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
