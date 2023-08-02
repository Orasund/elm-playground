module View.TileSelect exposing (..)

import Cell exposing (Cell(..))
import Dict exposing (Dict)
import Game exposing (Game)
import Html exposing (Html)
import Layout
import Level exposing (Level)
import Stage exposing (SavedStage)
import View


toHtml :
    { removeTile : ( Int, Int ) -> msg
    , selected : ( Int, Int )
    , unselect : msg
    , game : Maybe Game
    , level : Level
    , placeModule : { moduleId : Int, rotation : Int } -> msg
    , levels : Dict String (Dict Int SavedStage)
    }
    -> Dict Int SavedStage
    -> List (Html msg)
toHtml args dict =
    [ "Select a tile you want to place"
        |> View.cardTitle
    , dict
        |> Dict.keys
        |> List.map
            (\id ->
                List.range 0 3
                    |> List.map
                        (\rotate ->
                            { moduleId = id
                            , rotation = rotate
                            , sendsTo = Dict.empty
                            }
                                |> ConnectionCell
                                |> View.tileGeneric
                                    { level = args.level
                                    , connectedPathIds = []
                                    }
                                    (args.levels
                                        |> Dict.get (args.level |> Level.previous |> Level.toString)
                                        |> Maybe.withDefault Dict.empty
                                    )
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
