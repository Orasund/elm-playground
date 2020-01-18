module FactoryCity.Data.CellType exposing
    ( CellType
    , ContainerSort(..)
    , Item(..)
    , belt
    , burnable
    , color
    , containerList
    , crate
    , furnace
    , itemList
    , smeltable
    , toString
    )

import Color exposing (Color)
import Grid.Direction as Direction exposing (Direction(..))
import Jsonstore exposing (Json)


type Item
    = Wood
    | Stone
    | Iron


itemList : List Item
itemList =
    [ Wood, Stone, Iron ]


color : Item -> ( Int, Int, Int )
color item =
    case item of
        Wood ->
            ( 255, 194, 170 )

        Stone ->
            ( 117, 175, 150 )

        Iron ->
            ( 102, 153, 153 )


burnable : List Item
burnable =
    [ Wood ]


smeltable : List ( Item, Item )
smeltable =
    [ ( Stone, Iron ) ]


crate : Item -> CellType
crate item =
    { item = Just item, sort = Crate }


furnace : CellType
furnace =
    { item = Nothing
    , sort = Furnace { isWarm = False }
    }


belt : { from : Direction, to : Direction } -> CellType
belt { from, to } =
    { item = Nothing
    , sort = Belt { from = from, to = to }
    }


containerList : List ContainerSort
containerList =
    [ Crate
    , Furnace { isWarm = True }
    ]


type ContainerSort
    = Belt { from : Direction, to : Direction }
    | Crate
    | Furnace { isWarm : Bool }


type alias CellType =
    { item : Maybe Item, sort : ContainerSort }


itemToString : Item -> String
itemToString item =
    case item of
        Wood ->
            "wood"

        Stone ->
            "stone"

        Iron ->
            "iron"


directionToString : Direction -> String
directionToString dir =
    case dir of
        Up ->
            "🔼"

        Left ->
            "◀"

        Right ->
            "▶"

        Down ->
            "🔽"


toString : CellType -> ( String, String )
toString { sort, item } =
    ( case sort of
        Belt { from, to } ->
            [ from |> Direction.flip, to ]
                |> List.map directionToString
                |> String.concat

        Crate ->
            "📦"

        Furnace { isWarm } ->
            if isWarm then
                "🔥"

            else
                "📛"
    , item
        |> Maybe.map itemToString
        |> Maybe.withDefault ""
    )
