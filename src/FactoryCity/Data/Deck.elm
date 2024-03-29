module FactoryCity.Data.Deck exposing
    ( Deck
    , add
    , init
    , remove
    , toList
    )

import Bag exposing (Bag)
import Direction exposing (Direction(..))
import FactoryCity.Data.CellType as CellType exposing (ContainerSort)
import FactoryCity.Data.Item exposing (Item(..))
import List.Zipper exposing (Zipper(..))


type alias Deck =
    Bag String


init : Deck
init =
    [ CellType.crate Wood
    , CellType.crate Wood
    , CellType.crate Stone
    , CellType.crate Stone
    , CellType.furnace
    , CellType.belt { from = Up, to = Left }
    , CellType.belt { from = Up, to = Right }
    ]
        |> List.map (CellType.containerSortToString >> (\k -> ( k, 1 )))
        |> Bag.fromList


add : ContainerSort -> Deck -> Deck
add k =
    Bag.insert 1 (k |> CellType.containerSortToString)


remove : ContainerSort -> Int -> Deck -> Result () Deck
remove k n bag =
    if (bag |> Bag.count (k |> CellType.containerSortToString)) < n then
        Err ()

    else
        bag |> Bag.remove n (k |> CellType.containerSortToString) |> Ok


toList : Deck -> List ( ContainerSort, Int )
toList =
    Bag.toList
        >> List.filterMap
            (\( k, v ) ->
                k
                    |> CellType.stringToContainerSort
                    |> Maybe.map (\c -> ( c, v ))
            )
