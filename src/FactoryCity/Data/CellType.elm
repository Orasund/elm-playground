module FactoryCity.Data.CellType exposing
    ( CellType
    , ContainerSort(..)
    , MachineSort(..)
    , MovableSort(..)
    , RemovableSort(..)
    , belt
    , containerList
    , containerSortToString
    , craftingCost
    , crate
    , defaultTier
    , fromCard
    , fromString
    , furnace
    , machineList
    , merger
    , movableList
    , output
    , removableList
    , stringToContainerSort
    , tierOne
    , tierOneList
    , tierThree
    , tierThreeList
    , tierTwo
    , tierTwoList
    , toCard
    , toString
    )

import Bag exposing (Bag)
import FactoryCity.Data.Item as Item exposing (Item(..))
import Grid.Direction as Direction exposing (Direction(..))


dirList : List Direction
dirList =
    [ Up, Left, Down, Right ]


type MovableSort
    = Belt
    | Merger


movableList : List MovableSort
movableList =
    [ Belt, Merger ]


type MachineSort
    = Furnace
    | Shredder
    | Press


machineList : List MachineSort
machineList =
    [ Furnace
    , Shredder
    , Press
    ]


type RemovableSort
    = Bug
    | Trash


removableList : List RemovableSort
removableList =
    [ Bug, Trash ]


crate : Item -> ContainerSort
crate item =
    Crate item


output : ContainerSort
output =
    Output


furnace : ContainerSort
furnace =
    Machine Furnace { isWarm = False }


shredder : ContainerSort
shredder =
    Machine Shredder { isWarm = False }


press : ContainerSort
press =
    Machine Press { isWarm = False }


belt : { from : Direction, to : Direction } -> ContainerSort
belt { from, to } =
    Movable Belt { from = from, to = to }


merger : Direction -> ContainerSort
merger dir =
    Movable Merger { from = dir |> Direction.flip, to = dir }


containerList : List ContainerSort
containerList =
    List.concat
        [ Item.itemList |> List.map Crate
        , [ Machine Furnace { isWarm = True }
          , Machine Shredder { isWarm = True }
          , Machine Press { isWarm = False }
          ]
        ]


type ContainerSort
    = Movable MovableSort { from : Direction, to : Direction }
    | Crate Item
    | Machine MachineSort { isWarm : Bool }
    | Output
    | Removable RemovableSort


type alias CellType =
    { item : Maybe Item
    , sort : ContainerSort
    }


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
            Just "⤶"

        ( Up, Down ) ->
            Just "↓"

        ( Up, Right ) ->
            Just "⤷"

        ( Left, Up ) ->
            Just "⤴"

        ( Left, Right ) ->
            Just "→"

        ( Left, Down ) ->
            Just "⤵"

        ( Down, Up ) ->
            Just "↑"

        ( Down, Left ) ->
            Just "↰"

        ( Down, Right ) ->
            Just "↱"

        ( Right, Up ) ->
            Just "⬑"

        ( Right, Left ) ->
            Just "←"

        ( Right, Down ) ->
            Just "⬐"

        _ ->
            Nothing


stringToMovable : String -> Maybe { from : Direction, to : Direction }
stringToMovable string =
    (case string of
        "⤶" ->
            Just ( Up, Left )

        "↓" ->
            Just ( Up, Down )

        "⤷" ->
            Just ( Up, Right )

        "⤴" ->
            Just ( Left, Up )

        "→" ->
            Just ( Left, Right )

        "⤵" ->
            Just ( Left, Down )

        "↑" ->
            Just ( Down, Up )

        "↰" ->
            Just ( Down, Left )

        "↱" ->
            Just ( Down, Right )

        "⬑" ->
            Just ( Right, Up )

        "←" ->
            Just ( Right, Left )

        "⬐" ->
            Just ( Right, Down )

        _ ->
            Nothing
    )
        |> Maybe.map (\( from, to ) -> { from = from, to = to })


machineToString : MachineSort -> String
machineToString machineSort =
    case machineSort of
        Furnace ->
            "🔥"

        Shredder ->
            "⚒"

        Press ->
            "🗜"


stringToMachine : String -> Maybe MachineSort
stringToMachine string =
    case string of
        "🔥" ->
            Just Furnace

        "⚒" ->
            Just Shredder

        "🗜" ->
            Just Press

        _ ->
            Nothing


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
            "📦" ++ (item |> Item.itemToString)

        Machine machineSort { isWarm } ->
            (if isWarm then
                "🔄"

             else
                "❌"
            )
                ++ (machineSort |> machineToString)

        Output ->
            "🚛"

        Removable removableSort ->
            case removableSort of
                Bug ->
                    "🐞"

                Trash ->
                    "🗑"


stringToContainerSort : String -> Maybe ContainerSort
stringToContainerSort string =
    case string |> stringToMovable of
        Just { from, to } ->
            Just <| Movable Belt { from = from, to = to }

        Nothing ->
            case string |> String.uncons of
                Just ( '📦', item ) ->
                    item
                        |> Item.stringToItem
                        |> Maybe.map Crate

                Just ( '🔄', machine ) ->
                    machine
                        |> stringToMachine
                        |> Maybe.map (\m -> Machine m { isWarm = True })

                Just ( '❌', machine ) ->
                    machine
                        |> stringToMachine
                        |> Maybe.map (\m -> Machine m { isWarm = False })

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
                                "🚛" ->
                                    Just <| Output

                                "🐞" ->
                                    Just <| Removable Bug

                                "🗑" ->
                                    Just <| Removable Trash

                                _ ->
                                    Nothing


toString : CellType -> ( String, String )
toString { sort, item } =
    ( sort |> containerSortToString
    , item
        |> Maybe.map Item.itemToString
        |> Maybe.withDefault ""
    )


fromString : ( String, String ) -> Maybe CellType
fromString ( sortString, itemString ) =
    ( sortString |> stringToContainerSort
    , if itemString == "" then
        Just Nothing

      else
        itemString |> Item.stringToItem |> Maybe.map Just
    )
        |> (\( maybeSort, maybeItem ) ->
                case ( maybeSort, maybeItem ) of
                    ( Just sort, Just item ) ->
                        Just { sort = sort, item = item }

                    _ ->
                        Nothing
           )


toCard : CellType -> ContainerSort
toCard { sort } =
    case sort of
        Movable Belt movable ->
            belt movable

        Movable Merger { to } ->
            merger to

        Crate i ->
            crate i

        Machine Furnace _ ->
            furnace

        Machine Shredder _ ->
            shredder

        Machine Press _ ->
            press

        Output ->
            output

        Removable _ ->
            Crate Scrap


fromCard : ContainerSort -> CellType
fromCard containerSort =
    { item = Nothing
    , sort =
        case containerSort of
            Movable Belt movable ->
                belt movable

            Movable Merger { to } ->
                merger to

            Crate i ->
                crate i

            Machine Furnace _ ->
                furnace

            Machine Shredder _ ->
                shredder

            Machine Press _ ->
                press

            Output ->
                output

            Removable bug ->
                Removable bug
    }


defaultTier : List ( Item, Int )
defaultTier =
    []


tierOne : List ( Item, Int )
tierOne =
    [ ( Iron, 2 ) ]


tierOneList : List ContainerSort
tierOneList =
    [ Machine Furnace { isWarm = False }
    , Movable Belt { from = Up, to = Down }
    ]


tierTwo : List ( Item, Int )
tierTwo =
    [ ( Iron, 5 ) ]


tierTwoList : List ContainerSort
tierTwoList =
    List.concat
        [ [ Machine Shredder { isWarm = False }
          , Machine Press { isWarm = False }
          , output
          , Movable Merger { from = Up, to = Down }
          ]
        , dirList
            |> List.filterMap
                (\to ->
                    if to /= Down then
                        Just <| Movable Belt { from = to |> Direction.flip, to = to }

                    else
                        Nothing
                )
        ]


tierThree : List ( Item, Int )
tierThree =
    [ ( Iron, 2 ), ( Chipboard, 5 ) ]


tierThreeList : List ContainerSort
tierThreeList =
    List.concat
        [ dirList
            |> List.concatMap
                (\from ->
                    dirList
                        |> List.filterMap
                            (\to ->
                                if from == to then
                                    Nothing

                                else if from == (to |> Direction.flip) then
                                    Nothing

                                else
                                    Just <| Movable Belt { from = from, to = to }
                            )
                )
        , dirList
            |> List.filterMap
                (\to ->
                    if to /= Down then
                        Just <| Movable Merger { from = to |> Direction.flip, to = to }

                    else
                        Nothing
                )
        ]


craftingCost : ContainerSort -> Bag String
craftingCost card =
    (case card of
        Crate _ ->
            defaultTier

        Movable Belt { from, to } ->
            if from == to then
                defaultTier

            else if from == (to |> Direction.flip) then
                tierOne

            else
                tierThree

        Movable Merger { to } ->
            if to == Down then
                tierTwo

            else
                tierThree

        Machine Furnace _ ->
            tierOne

        Machine Shredder _ ->
            tierTwo

        Machine Press _ ->
            tierTwo

        Output ->
            tierTwo

        Removable _ ->
            defaultTier
    )
        |> List.map (Tuple.mapFirst (crate >> containerSortToString))
        |> Bag.fromList
