module FactoryCity.Data.Deck exposing
    ( Deck
    , add
    , init
    , remove
    , toList
    )

import Bag exposing (Bag)
import Dict exposing (Dict)
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort, Item(..))
import Grid.Direction as Direction exposing (Direction(..))
import Jsonstore exposing (Json)
import List.Zipper as Zipper exposing (Zipper(..))
import Random exposing (Generator)
import Random.List as RandomList


type alias Deck =
    Bag String


init : Deck
init =
    [ CellType.crate Wood
    , CellType.crate Stone
    , CellType.furnace
    , CellType.belt { from = Up, to = Down }
    , CellType.merger Down
    , CellType.merger Left
    , CellType.output
    ]
        |> List.map (CellType.containerSortToString >> (\k -> ( k, 1 )))
        |> Bag.fromList


add : ContainerSort -> Deck -> Deck
add k =
    Bag.insert 1 (k |> CellType.containerSortToString)


remove : ContainerSort -> Deck -> Result () Deck
remove k bag =
    if (bag |> Bag.count (k |> CellType.containerSortToString)) < 1 then
        Err ()

    else
        bag |> Bag.remove 1 (k |> CellType.containerSortToString) |> Ok


toList : Deck -> List ( ContainerSort, Int )
toList =
    Bag.toList
        >> List.filterMap
            (\( k, v ) ->
                k
                    |> CellType.stringToContainerSort
                    |> Maybe.map (\c -> ( c, v ))
            )
