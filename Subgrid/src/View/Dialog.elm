module View.Dialog exposing (..)

import Cell exposing (Cell(..))
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import RelativePos
import Set
import Stage exposing (SavedStage)
import StaticArray.Index as Index
import View
import View.Render
import View.Svg


type alias Dialog msg =
    { content : List (Html msg)
    , dismiss : Maybe msg
    }


tileSelect :
    { removeTile : ( Int, Int ) -> msg
    , selected : ( Int, Int )
    , unselect : msg
    , game : Maybe Game
    , level : Level
    , placeModule : { moduleId : Int, rotation : Int } -> msg
    , levels : Dict String (Dict Int SavedStage)
    , cellSize : Int
    }
    -> Dict Int SavedStage
    -> Dialog msg
tileSelect args dict =
    [ "Select Tile" |> View.cardTitle
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
                                    { cellSize = Config.defaultCellSize
                                    , active =
                                        \pos ->
                                            level.paths
                                                |> Dict.get (RelativePos.fromTuple pos)
                                                |> Maybe.withDefault Set.empty
                                                |> Set.toList
                                                |> List.head
                                                |> (\originId -> { originId = originId })
                                    , render = \_ -> View.Render.boxRender
                                    , level = args.level
                                    }
                                |> Layout.el
                                    [ Html.Attributes.style "transform"
                                        ("rotate(" ++ String.fromInt (rotate * 90) ++ "deg)")
                                    ]
                                |> Layout.el
                                    (Layout.asButton
                                        { label = "Level " ++ String.fromInt id ++ "(" ++ String.fromInt rotate ++ ")"
                                        , onPress = Just (args.placeModule { moduleId = id, rotation = rotate })
                                        }
                                    )
                        )
                    |> Layout.row [ Layout.gap 8 ]
            )
        |> Layout.column [ Layout.gap 8 ]
    , (if
        args.game
            |> Maybe.map (\game -> game.stage.grid)
            |> Maybe.withDefault Dict.empty
            |> Dict.member args.selected
       then
        [ View.button (args.removeTile args.selected) "Remove" ]

       else
        []
      )
        ++ [ View.primaryButton args.unselect "Cancel" ]
        |> Layout.row [ Layout.gap 8 ]
    ]
        |> (\list -> { content = list, dismiss = Just args.unselect })


levelSelect :
    { load : { level : Level, stage : Int } -> msg
    , levels : Dict String (Dict Int SavedStage)
    , dismiss : msg
    }
    -> Dialog msg
levelSelect args =
    [ "Edit Levels" |> View.cardTitle
    , Index.range Level.maxLevel
        |> List.reverse
        |> List.map
            (\level ->
                args.levels
                    |> Dict.get (level |> Level.toString)
                    |> Maybe.withDefault Dict.empty
                    |> View.savedLevels { level = level }
                        (\stage ->
                            args.load
                                { level = level
                                , stage = stage
                                }
                        )
            )
        |> Layout.column [ Layout.gap 8 ]
    ]
        |> (\list -> { content = list, dismiss = Just args.dismiss })


levelSolved :
    { level : Level
    , levels : Dict String (Dict Int SavedStage)
    , game : Maybe Game
    , stage : Int
    , nextStage : msg
    }
    -> Dialog msg
levelSolved args =
    [ View.stageName { level = args.level, stage = args.stage }
        ++ " Complete"
        |> View.cardTitle
    , View.game []
        { levels =
            args.levels
                |> Dict.get (args.level |> Level.previous |> Level.toString)
                |> Maybe.withDefault Dict.empty
        , onToggle = \_ -> Nothing
        , level = args.level
        , cellSize = Config.smallCellSize
        }
        args.game
    , View.primaryButton args.nextStage
        "Next Level"
    ]
        |> (\list -> { content = list, dismiss = Nothing })
