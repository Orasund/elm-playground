module Farmig.View.Grid exposing (view)

import Color
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Farmig.Data.Cell as Cell exposing (Cell(..))
import Farmig.Data.Item as Item exposing (Item)
import Farmig.View.Cell as Cell
import Widget
import Widget.Customize as Customize
import Widget.Material as Material


firstIndex : { screenSize : Int, playerIndex : Int } -> Int
firstIndex { screenSize, playerIndex } =
    playerIndex - screenSize // 2


view : { screenSize : Int, food : Int, item : Maybe Item, player : { x : Int, y : Int } } -> Dict ( Int, Int ) Cell -> List (Widget.Item msg)
view { screenSize, food, item, player } dict =
    let
        minX =
            firstIndex
                { screenSize = screenSize
                , playerIndex = player.x
                }

        minY =
            firstIndex
                { screenSize = screenSize
                , playerIndex = player.y
                }
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
                            if (player.x == x) && (player.y == y) then
                                Cell.viewPlayer
                                    { food = food
                                    , item = item
                                    }

                            else
                                dict
                                    |> Dict.get ( x, y )
                                    |> Cell.view
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
