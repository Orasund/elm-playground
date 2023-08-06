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
import Set
import Stage exposing (SavedStage)
import StaticArray.Index as Index
import View.Render
import View.Svg


tileSelect :
    { selected : Maybe { moduleId : Int, rotation : Int }
    , unselect : msg
    , game : Maybe Game
    , level : Level
    , selectTile : { moduleId : Int, rotation : Int } -> msg
    , levels : Dict String (Dict Int SavedStage)
    , cellSize : Int
    , clearStage : msg
    }
    -> Dict Int SavedStage
    -> List (Html msg)
tileSelect args dict =
    case args.selected of
        Just { moduleId, rotation } ->
            if args.selected /= Nothing then
                [ "Select Position" |> cardTitle
                , "Click on the position where you want to place the tile" |> Layout.text []
                , dict
                    |> Dict.get moduleId
                    |> Maybe.map
                        (\level ->
                            level.grid
                                |> View.Svg.tile
                                    { cellSize = Config.bigCellSize
                                    , active =
                                        \pos ->
                                            level.paths
                                                |> Dict.get (RelativePos.fromTuple pos)
                                                |> Maybe.withDefault { origins = Set.empty }
                                                |> .origins
                                                |> Set.toList
                                                |> List.head
                                                |> (\originId -> { originId = originId })
                                    , render = \_ -> View.Render.boxRender
                                    , level = args.level
                                    , background = Color.tileBackground
                                    }
                                |> Layout.el
                                    [ Html.Attributes.style "transform"
                                        ("rotate(" ++ String.fromInt (rotation * 90) ++ "deg)")
                                    ]
                                |> Layout.el []
                        )
                    |> Maybe.withDefault Layout.none
                , button args.unselect "Cancel"
                ]

            else
                []

        Nothing ->
            [ [ "Select Tile" |> cardTitle
              , button args.clearStage "Reset Level"
              ]
                |> Layout.row [ Layout.contentWithSpaceBetween ]
            , "Select a tile you want to place" |> Layout.text []
            , dict
                |> Dict.toList
                |> List.map
                    (\( id, level ) ->
                        List.range 0 3
                            |> List.map
                                (\rotate ->
                                    level.grid
                                        |> View.Svg.tile
                                            { cellSize = Config.smallCellSize
                                            , active =
                                                \pos ->
                                                    level.paths
                                                        |> Dict.get (RelativePos.fromTuple pos)
                                                        |> Maybe.map .origins
                                                        |> Maybe.withDefault Set.empty
                                                        |> Set.toList
                                                        |> List.head
                                                        |> (\originId -> { originId = originId })
                                            , render = \_ -> View.Render.boxRender
                                            , level = args.level
                                            , background = Color.tileBackground
                                            }
                                        |> Layout.el
                                            [ Html.Attributes.style "transform"
                                                ("rotate(" ++ String.fromInt (rotate * 90) ++ "deg)")
                                            ]
                                        |> Layout.el
                                            (Layout.asButton
                                                { label = "Level " ++ String.fromInt id ++ "(" ++ String.fromInt rotate ++ ")"
                                                , onPress = Just (args.selectTile { moduleId = id, rotation = rotate })
                                                }
                                            )
                                )
                            |> Layout.row [ Layout.gap 8 ]
                    )
                |> Layout.column [ Layout.gap 8 ]
            ]


topBar : { level : Level, stage : Int } -> Html msg -> Html msg
topBar args content =
    [ stageName { level = args.level, stage = args.stage }
        |> title
    , content
    ]
        |> Layout.row [ Layout.contentWithSpaceBetween ]


gameWon : Html msg
gameWon =
    "You Win!"
        |> Layout.text []
        |> Layout.el
            ([ Html.Attributes.style "background" "linear-gradient(45deg,orange, yellow)"
             , Html.Attributes.style "width" ((Config.bigCellSize * 6 |> String.fromInt) ++ "px")
             , Html.Attributes.style "height" ((Config.bigCellSize * 6 |> String.fromInt) ++ "px")
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
                    |> View.Svg.tile
                        { cellSize = Config.bigCellSize
                        , active =
                            \pos ->
                                level.paths
                                    |> Dict.get (RelativePos.fromTuple pos)
                                    |> Maybe.map .origins
                                    |> Maybe.withDefault Set.empty
                                    |> Set.toList
                                    |> List.head
                                    |> (\originId -> { originId = originId })
                        , render = \_ -> View.Render.boxRender
                        , level = args.level
                        , background = Color.tileBackground
                        }
                , Layout.text [] (stageName { level = args.level, stage = id })
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


stageName : { level : Level, stage : Int } -> String
stageName args =
    "Level " ++ Level.toString args.level ++ " - " ++ String.fromInt args.stage


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


tileLevel1 : { level : Level, amount : Int, cellSize : Int } -> Cell -> Html msg
tileLevel1 args cell =
    Cell.toColor
        { level = args.level
        }
        Nothing
        cell
        |> View.Svg.singleCell
            { cellSize = args.cellSize
            , render =
                View.Render.cellRender
                    cell
            , background = Color.tileBackground
            }


tileGeneric : { level : Level, cellSize : Int } -> Dict Int SavedStage -> Cell -> Html msg
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
                            |> View.Svg.tile
                                { cellSize = args.cellSize
                                , active =
                                    \pos ->
                                        activePos
                                            |> Dict.get (RelativePos.fromTuple pos)
                                            |> Maybe.withDefault { originId = Nothing }
                                , render = \_ -> View.Render.boxRender
                                , level = args.level
                                , background = Color.tileBackground
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
                |> View.Svg.singleCell
                    { cellSize = args.cellSize
                    , render =
                        View.Render.cellRender
                            cell
                    , background = Color.tileBackground
                    }


game :
    List (Attribute msg)
    ->
        { levels : Dict Int SavedStage
        , onToggle : ( Int, Int ) -> Maybe msg
        , level : Level
        , cellSize : Int
        }
    -> Maybe Game
    -> Html msg
game attrs args maybeGame =
    maybeGame
        |> Maybe.map
            (\g ->
                List.range -1 4
                    |> List.map
                        (\y ->
                            List.range -1 4
                                |> List.map
                                    (\x ->
                                        g
                                            |> gamePos
                                                (Layout.asButton
                                                    { onPress = args.onToggle ( x, y )
                                                    , label = "Toggle " ++ String.fromInt x ++ "," ++ String.fromInt y
                                                    }
                                                )
                                                { pos = ( x, y )
                                                , levels = args.levels
                                                , level = args.level
                                                , cellSize = args.cellSize
                                                }
                                    )
                                |> Layout.row [ Layout.noWrap ]
                        )
                    |> Layout.column attrs
            )
        |> Maybe.withDefault gameWon
        |> Layout.el
            [ Html.Attributes.style "border-radius" "1rem"
            , Html.Attributes.style "overflow" "hidden"
            ]


gamePos :
    List (Attribute msg)
    ->
        { pos : ( Int, Int )
        , levels : Dict Int SavedStage
        , level : Level
        , cellSize : Int
        }
    -> Game
    -> Html msg
gamePos attrs args g =
    (if args.level == Index.first then
        g.stage.grid
            |> Dict.get args.pos
            |> Maybe.map
                (tileLevel1
                    { level = args.level
                    , amount = 0
                    , cellSize = args.cellSize
                    }
                )

     else
        g.stage.grid
            |> Dict.get args.pos
            |> Maybe.map
                (tileGeneric
                    { level = args.level
                    , cellSize = args.cellSize
                    }
                    args.levels
                )
    )
        |> Maybe.withDefault Layout.none
        |> Layout.el
            ([ Html.Attributes.style "width" (String.fromInt args.cellSize ++ "px")
             , Html.Attributes.style "height" (String.fromInt args.cellSize ++ "px")
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
