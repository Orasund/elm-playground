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
    , movableList
    , MovableSort(..)
    , merger
    , output
    )

import Color exposing (Color)
import Grid.Direction as Direction exposing (Direction(..))
import Jsonstore exposing (Json)


type Item
    = Wood
    | Stone
    | Iron

type MovableSort
    = Belt
    | Merger

movableList : List MovableSort
movableList =
    [ Belt , Merger ]

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

output : CellType
output =
    {item = Nothing, sort = Output}

furnace : CellType
furnace =
    { item = Nothing
    , sort = Furnace { isWarm = False }
    }


belt : { from : Direction, to : Direction } -> CellType
belt { from, to } =
    { item = Nothing
    , sort = Movable Belt { from = from, to = to }
    }

merger : Direction -> CellType
merger dir =
    { item = Nothing
    , sort = Movable Merger {from = dir |> Direction.flip, to = dir }
    }

containerList : List ContainerSort
containerList =
    [ Crate
    , Furnace { isWarm = True }
    ]


type ContainerSort
    = Movable MovableSort { from : Direction, to : Direction }
    | Crate
    | Furnace { isWarm : Bool }
    | Output


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
        Movable movableSort { from, to } ->
            case movableSort of
                Belt ->
                    case (from,to) of
                        (Up,Left)->
                            "↵"
                        (Up,Down) ->
                            "↓"
                        (Up,Right) ->
                            "↪"
                        (Left,Up) ->
                            "⤴"
                        (Left,Right) ->
                            "→"
                        (Left,Down) ->
                            "⤵"
                        (Down,Up) ->
                            "↑"
                        (Down,Left) ->
                            "⮢"
                        (Down,Right) ->
                            "⮣"
                        (Right,Up) ->
                            "⮤"
                        (Right,Left) ->
                            "←"
                        (Right,Down) ->
                            "⮦"
                        _ ->
                            "🔄"

                Merger ->
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
        
        Output ->
            "🚚"
    , item
        |> Maybe.map itemToString
        |> Maybe.withDefault ""
    )
