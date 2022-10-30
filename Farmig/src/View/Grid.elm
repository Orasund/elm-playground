module  View.Grid exposing (view)

import Color
import Dict exposing (Dict)
import Element
import Element.Border as Border
import  Data.Cell as Cell exposing (Cell(..))
import  Data.Item exposing (Item)
import  View.Cell as Cell
import Widget


firstIndex : { screenSize : Int, playerIndex : Int } -> Int
firstIndex { screenSize, playerIndex } =
    playerIndex - screenSize // 2


view :
    { screenSize : Int
    , food : Int
    , tree : String
    , item : Maybe Item
    , player : { x : Int, y : Int }
    , onPress : ( Int, Int ) -> msg
    }
    -> Dict ( Int, Int ) Cell
    -> List (Widget.Item msg)
view { screenSize, tree, food, item, player, onPress } dict =
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
                                    |> Cell.view tree
                                        (if abs (x - player.x) + abs (y - player.y) == 1 then
                                            Just <| onPress ( x - player.x, y - player.y )

                                         else
                                            Nothing
                                        )
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
