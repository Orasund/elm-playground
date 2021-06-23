module Bomb16.View.Grid exposing (view)

import Color
import Dict exposing (Dict)
import Bomb16.Data.Cell as Cell exposing (Cell(..))
import Bomb16.Data.Input exposing (Direction)
import Bomb16.Data.World as World
import Bomb16.View.Cell as Cell
import Element
import Element.Border as Border
import Widget


firstIndex : { screenSize : Int, playerIndex : Int } -> Int
firstIndex { screenSize, playerIndex } =
    playerIndex - screenSize // 2


view :
    { food : Int
    , tree : String
    , onPress : Direction -> msg
    }
    -> Dict ( Int, Int ) Cell
    -> List (Widget.Item msg)
view { tree, food, onPress } dict =
    let
        screenSize =
            World.size

        minX =
            0

        minY =
            0
    in
    List.range
        minY
        (minY + screenSize - 1)
        |> List.map
            (\y ->
                List.range
                    minX
                    (minX + screenSize - 1)
                    |> List.map
                        (\x ->
                            dict
                                |> Dict.get ( x, y )
                                |> Cell.view tree Nothing
                        )
                    |> Widget.row
                        { elementRow = []
                        , content =
                            { element =
                                [ Color.gray
                                    |> Color.toRgba
                                    |> Element.fromRgb
                                    |> Border.color
                                ]
                            , ifFirst =
                                [ Border.widthEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 1
                                    , top = 0
                                    }
                                ]
                            , ifLast =
                                [ Border.width 0
                                ]
                            , ifSingleton =
                                [ Border.width 0
                                ]
                            , otherwise =
                                [ Border.widthEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 1
                                    , top = 0
                                    }
                                ]
                            }
                        }
                    |> Widget.asItem
            )
