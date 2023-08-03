module View exposing (..)

import Cell exposing (Cell(..))
import Color
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import RelativePos exposing (RelativePos)
import Set exposing (Set)
import Stage exposing (SavedStage)
import StaticArray.Index as Index
import View.Svg exposing (RenderFunction)


topBar : { level : Level, stage : Int, clearStage : msg } -> Html msg
topBar args =
    [ "Level " ++ Level.toString args.level ++ " - " ++ String.fromInt args.stage |> title
    , button args.clearStage "Reset Level"
    ]
        |> Layout.row [ Layout.contentWithSpaceBetween ]


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
                        , active =
                            \pos ->
                                level.paths
                                    |> Dict.get (RelativePos.fromTuple pos)
                                    |> Maybe.withDefault Set.empty
                                    |> Set.toList
                                    |> List.head
                                    |> (\originId -> { originId = originId })
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


tileLevel1 : { level : Level, amount : Int, connectedPathIds : List Int } -> Cell -> Html msg
tileLevel1 args cell =
    Cell.toColor
        { level = args.level
        }
        Nothing
        cell
        |> View.Svg.cell1
            { height = Config.cellSize
            , width = Config.cellSize
            , render =
                cellRender
                    { level = args.level
                    , connectedPathIds = args.connectedPathIds
                    }
                    cell
            }


tileGeneric : { level : Level, connectedPathIds : List Int } -> Dict Int SavedStage -> Cell -> Html msg
tileGeneric args g cell =
    case cell of
        ConnectionCell c ->
            g
                |> Dict.get c.moduleId
                |> Maybe.map
                    (\level ->
                        let
                            activePos : Dict RelativePos { originId : Maybe Int }
                            activePos =
                                c.sendsTo
                                    |> Dict.toList
                                    |> List.map (Tuple.mapFirst (RelativePos.rotate args.level (4 - c.rotation)))
                                    |> List.concatMap
                                        (\( to, { originId } ) ->
                                            level.connections
                                                |> Dict.get to
                                                |> Maybe.map .path
                                                |> Maybe.withDefault []
                                                |> List.map (\p -> ( p, { originId = Just originId } ))
                                        )
                                    |> Dict.fromList
                        in
                        level.grid
                            |> View.Svg.grid
                                { height = Config.cellSize
                                , width = Config.cellSize
                                , active =
                                    \pos ->
                                        activePos
                                            |> Dict.get (RelativePos.fromTuple pos)
                                            |> Maybe.withDefault { originId = Nothing }
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
                |> Cell.toColor
                    { level = args.level
                    }
                    Nothing
                |> View.Svg.cell1
                    { height = Config.cellSize
                    , width = Config.cellSize
                    , render =
                        cellRender
                            { level = args.level
                            , connectedPathIds = args.connectedPathIds
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
    (if args.level == Index.first then
        g.stage.grid
            |> Dict.get args.pos
            |> Maybe.map
                (tileLevel1
                    { level = args.level
                    , amount = 0
                    , connectedPathIds =
                        g.isConnected
                            |> Dict.get (RelativePos.fromTuple args.pos)
                            |> Maybe.map .targetIds
                            |> Maybe.map Set.toList
                            |> Maybe.withDefault []
                    }
                )

     else
        g.stage.grid
            |> Dict.get args.pos
            |> Maybe.map
                (tileGeneric
                    { level = args.level
                    , connectedPathIds =
                        g.isConnected
                            |> Dict.get (RelativePos.fromTuple args.pos)
                            |> Maybe.map .targetIds
                            |> Maybe.map Set.toList
                            |> Maybe.withDefault []
                    }
                    args.levels
                )
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


cellRender : { level : Level, connectedPathIds : List Int } -> Cell -> RenderFunction msg
cellRender args cell =
    case cell of
        ConnectionCell _ ->
            View.Svg.boxRender

        Wall ->
            View.Svg.boxRender

        Origin _ ->
            case args.connectedPathIds of
                [ pathId ] ->
                    View.Svg.targetRender
                        { secondaryColor = Color.wallColor
                        , variant = pathId
                        , small = True
                        , fill = True
                        }

                _ ->
                    View.Svg.boxRender

        Target { id, sendsTo } ->
            case sendsTo |> Dict.toList of
                [ _ ] ->
                    View.Svg.targetRender
                        { secondaryColor = Color.wallColor
                        , variant = id
                        , small = False
                        , fill = True
                        }

                _ ->
                    View.Svg.targetRender
                        { secondaryColor = Color.wallColor
                        , variant = id
                        , small = False
                        , fill = False
                        }


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
