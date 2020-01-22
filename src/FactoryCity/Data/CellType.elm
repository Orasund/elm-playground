module FactoryCity.Data.CellType exposing
    ( CellType
    , ContainerSort(..)
    , Item(..)
    , MovableSort(..)
    , belt
    , burnable
    , color
    , containerList
    , containerSortToString
    , crate
    , fromCard
    , fromString
    , furnace
    , itemList
    , merger
    , movableList
    , output
    , smeltable
    , stringToContainerSort
    , toCard
    , toString
    )

import Color exposing (Color)
import Grid.Direction as Direction exposing (Direction(..))
import Jsonstore exposing (Json)


type Item
    = Wood
    | Stone
    | Iron
    | Scrap


type MovableSort
    = Belt
    | Merger


movableList : List MovableSort
movableList =
    [ Belt, Merger ]


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

        Scrap ->
            ( 255, 170, 170 )


burnable : List Item
burnable =
    [ Wood ]


smeltable : List ( Item, Item )
smeltable =
    [ ( Stone, Iron ) ]


crate : Item -> ContainerSort
crate item =
    Crate item


output : ContainerSort
output =
    Output


furnace : ContainerSort
furnace =
    Furnace { isWarm = False }


belt : { from : Direction, to : Direction } -> ContainerSort
belt { from, to } =
    Movable Belt { from = from, to = to }


merger : Direction -> ContainerSort
merger dir =
    Movable Merger { from = dir |> Direction.flip, to = dir }


containerList : List ContainerSort
containerList =
    List.concat
        [ itemList |> List.map Crate
        , [ Furnace { isWarm = True }
          ]
        ]


type ContainerSort
    = Movable MovableSort { from : Direction, to : Direction }
    | Crate Item
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

        Scrap ->
            "scrap"


stringToItem : String -> Maybe Item
stringToItem string =
    case string of
        "wood" ->
            Just Wood

        "stone" ->
            Just Stone

        "iron" ->
            Just Iron

        "scrap" ->
            Just Scrap

        _ ->
            Nothing


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


stringToDirection : String -> Maybe Direction
stringToDirection string =
    case string of
        "🔼" ->
            Just Up

        "◀" ->
            Just Left

        "▶" ->
            Just Right

        "🔽" ->
            Just Down

        _ ->
            Nothing


movableToString : { from : Direction, to : Direction } -> Maybe String
movableToString { from, to } =
    case ( from, to ) of
        ( Up, Left ) ->
            Just "↵"

        ( Up, Down ) ->
            Just "↓"

        ( Up, Right ) ->
            Just "↪"

        ( Left, Up ) ->
            Just "⤴"

        ( Left, Right ) ->
            Just "→"

        ( Left, Down ) ->
            Just "⤵"

        ( Down, Up ) ->
            Just "↑"

        ( Down, Left ) ->
            Just "⮢"

        ( Down, Right ) ->
            Just "⮣"

        ( Right, Up ) ->
            Just "⮤"

        ( Right, Left ) ->
            Just "←"

        ( Right, Down ) ->
            Just "⮦"

        _ ->
            Nothing


stringToMovable : String -> Maybe { from : Direction, to : Direction }
stringToMovable string =
    (case string of
        "↵" ->
            Just ( Up, Left )

        "↓" ->
            Just ( Up, Down )

        "↪" ->
            Just ( Up, Right )

        "⤴" ->
            Just ( Left, Up )

        "→" ->
            Just ( Left, Right )

        "⤵" ->
            Just ( Left, Down )

        "↑" ->
            Just ( Down, Up )

        "⮢" ->
            Just ( Down, Left )

        "⮣" ->
            Just ( Down, Right )

        "⮤" ->
            Just ( Right, Up )

        "←" ->
            Just ( Right, Left )

        "⮦" ->
            Just ( Right, Down )

        _ ->
            Nothing
    )
        |> Maybe.map (\( from, to ) -> { from = from, to = to })


containerSortToString : ContainerSort -> String
containerSortToString containerSort =
    case containerSort of
        Movable movableSort { from, to } ->
            case movableSort of
                Belt ->
                    { from = from, to = to }
                        |> movableToString
                        |> Maybe.withDefault ""

                Merger ->
                    [ from |> Direction.flip, to ]
                        |> List.map directionToString
                        |> String.concat

        Crate item ->
            "📦" ++ (item |> itemToString)

        Furnace { isWarm } ->
            if isWarm then
                "🔥"

            else
                "📛"

        Output ->
            "🚛"


stringToContainerSort : String -> Maybe ContainerSort
stringToContainerSort string =
    case string |> stringToMovable of
        Just { from, to } ->
            Just <| Movable Belt { from = from, to = to }

        Nothing ->
            case string |> String.uncons of
                Just ( '📦', item ) ->
                    item
                        |> stringToItem
                        |> Maybe.map Crate

                _ ->
                    case
                        string
                            |> String.toList
                            |> List.map (String.fromChar >> stringToDirection)
                    of
                        [ Just from, Just to ] ->
                            Just <| Movable Merger { from = from |> Direction.flip, to = to }

                        _ ->
                            case string of
                                "🔥" ->
                                    Just <| Furnace { isWarm = True }

                                "📛" ->
                                    Just <| Furnace { isWarm = False }

                                "🚛" ->
                                    Just <| Output

                                _ ->
                                    Nothing


toString : CellType -> ( String, String )
toString { sort, item } =
    ( sort |> containerSortToString
    , item
        |> Maybe.map itemToString
        |> Maybe.withDefault ""
    )


fromString : ( String, String ) -> Maybe CellType
fromString ( sortString, itemString ) =
    ( sortString |> stringToContainerSort
    , if itemString == "" then
        Just Nothing

      else
        itemString |> stringToItem |> Maybe.map Just
    )
        |> (\( maybeSort, maybeItem ) ->
                case ( maybeSort, maybeItem ) of
                    ( Just sort, Just item ) ->
                        Just { sort = sort, item = item }

                    _ ->
                        Nothing
           )


toCard : CellType -> ContainerSort
toCard { sort, item } =
    case sort of
        Movable Belt movable ->
            belt movable

        Movable Merger { to } ->
            merger to

        Crate i ->
            case item of
                Just _ ->
                    crate i

                Nothing ->
                    crate Scrap

        Furnace _ ->
            furnace

        Output ->
            output


fromCard : ContainerSort -> CellType
fromCard containerSort =
    case containerSort of
        Movable Belt movable ->
            { item = Nothing
            , sort = Movable Belt movable
            }

        Movable Merger movable ->
            { item = Nothing
            , sort = Movable Merger movable
            }

        Crate i ->
            { item = Just i
            , sort = Crate i
            }

        Furnace _ ->
            { item = Nothing
            , sort = Furnace { isWarm = False }
            }

        Output ->
            { item = Nothing
            , sort = output
            }
