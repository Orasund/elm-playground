module View exposing (..)

import Cell exposing (Cell(..))
import Color
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Level exposing (Level(..))
import RelativePos
import Set
import Stage exposing (SavedStage)
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


savedLevels : { level : Level } -> (Int -> msg) -> Dict Int SavedStage -> Html msg
savedLevels args fun dict =
    dict
        |> Dict.toList
        |> List.map
            (\( id, level ) ->
                [ level.grid
                    |> View.Svg.grid
                        { width = Config.cellSize
                        , height = Config.cellSize
                        , active = \pos -> level.paths |> Dict.get (RelativePos.fromTuple pos) |> Maybe.withDefault Set.empty |> Set.toList |> List.head
                        , render = \_ -> View.Svg.boxRender
                        , level = args.level
                        }
                , Layout.text [] ("Level " ++ Level.toString args.level ++ " - " ++ String.fromInt id)
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
         , Html.Attributes.style "background-color" Color.white
         , Html.Attributes.style "border-radius" "1rem"
         , Layout.gap 16
         ]
            ++ attrs
        )


tileLevel1 : { level : Level, amount : Int } -> Cell -> Html msg
tileLevel1 args cell =
    Cell.cell1ToColor args
        Nothing
        cell
        |> View.Svg.cell1
            { height = Config.cellSize
            , width = Config.cellSize
            , render = cellRender { laserColor = Color.laserColor args.level args.amount } cell
            }


tile2 : { level : Level } -> Dict Int SavedStage -> Cell -> Html msg
tile2 args g cell =
    case cell of
        ConnectionCell c ->
            g
                |> Dict.get c.moduleId
                |> Maybe.map
                    (\level ->
                        let
                            activePaths =
                                c.sendsTo
                                    |> Dict.keys
                                    |> List.map (RelativePos.rotate args.level (4 - c.rotation))
                                    |> List.filterMap
                                        (\to ->
                                            level.connections
                                                |> Dict.get to
                                                |> Maybe.map .pathId
                                        )
                                    |> Set.fromList
                        in
                        level.grid
                            |> View.Svg.grid
                                { height = Config.cellSize
                                , width = Config.cellSize
                                , active =
                                    \pos ->
                                        level.paths
                                            |> Dict.get (RelativePos.fromTuple pos)
                                            |> Maybe.withDefault Set.empty
                                            |> Set.toList
                                            |> List.filter (\p -> Set.member p activePaths)
                                            |> List.head
                                , render = \_ -> View.Svg.boxRender
                                , level = args.level
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
                    { level = args.level
                    , amount = 0
                    }
                    Nothing
                |> View.Svg.cell1
                    { height = Config.cellSize
                    , width = Config.cellSize
                    , render =
                        cellRender
                            { laserColor = Color.laserColor args.level 0
                            }
                            cell
                    }


grid :
    List (Attribute msg)
    ->
        { levels : Dict Int SavedStage
        , onToggle : ( Int, Int ) -> msg
        , level : Level
        }
    -> Game
    -> Html msg
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
                                    , level = args.level
                                    }
                        )
                    |> Layout.row [ Layout.noWrap ]
            )
        |> Layout.column attrs


game :
    List (Attribute msg)
    ->
        { pos : ( Int, Int )
        , levels : Dict Int SavedStage
        , level : Level
        }
    -> Game
    -> Html msg
game attrs args g =
    (case args.level of
        Level1 ->
            g.stage.grid
                |> Dict.get args.pos
                |> Maybe.map
                    (tileLevel1
                        { level = args.level
                        , amount = 0
                        }
                    )

        Level2 ->
            g.stage.grid
                |> Dict.get args.pos
                |> Maybe.map (tile2 { level = args.level } args.levels)

        Level3 ->
            g.stage.grid
                |> Dict.get args.pos
                |> Maybe.map (tile2 { level = args.level } args.levels)
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


cellRender : { laserColor : String } -> Cell -> RenderFunction msg
cellRender args cell =
    case cell of
        ConnectionCell _ ->
            View.Svg.boxRender

        Wall ->
            View.Svg.boxRender

        Origin ->
            View.Svg.boxRender

        Target Nothing ->
            View.Svg.targetRender { secondaryColor = args.laserColor, variant = 0 }

        Target (Just _) ->
            View.Svg.targetRender { secondaryColor = Color.wallColor, variant = 0 }


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
