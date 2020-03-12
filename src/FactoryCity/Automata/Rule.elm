module FactoryCity.Automata.Rule exposing (burnable, container, merger, movables, output, pressable, shreddable, smeltable)

import CellAutomata exposing (Rule, RuleExpression(..))
import FactoryCity.Automata.Neighborhood as Neighborhood
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..), MachineSort(..), MovableSort(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
import Grid.Direction as Direction exposing (Direction(..))


directionList : List Direction
directionList =
    [ Up, Right, Down, Left ]


output : List (Rule CellType)
output =
    let
        rules : MovableSort -> { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules movableSort { from, to } item =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Output
                        }
              , to =
                    Just
                        { item = Just item
                        , sort = Output
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (to |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { item = Just item
                                        , sort = Movable movableSort { from = from, to = to }
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just item
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set to <|
                                Exactly <|
                                    Just
                                        { item = Nothing
                                        , sort = Output
                                        }
                           )
              }
            ]
    in
    CellType.movableList
        |> List.concatMap
            (\movableSort ->
                directionList
                    |> List.concatMap
                        (\from ->
                            directionList
                                |> List.concatMap
                                    (\to ->
                                        Item.itemList
                                            |> List.concatMap
                                                (\i -> rules movableSort { from = from, to = to } i)
                                    )
                        )
            )


merger : List (Rule CellType)
merger =
    let
        rules : { from : Direction, to : Direction } -> ( Item, Item ) -> List (Rule CellType)
        rules { from, to } _ =
            [ { from =
                    Just <|
                        { item = Nothing
                        , sort = Movable Merger { from = from, to = to }
                        }
              , to =
                    Just <|
                        { item = Nothing
                        , sort =
                            Movable Merger
                                { from =
                                    from
                                        |> Direction.rotRight
                                        |> (\x ->
                                                if x == to then
                                                    x |> Direction.rotRight

                                                else
                                                    x
                                           )
                                , to = to
                                }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
              }
            ]
    in
    directionList
        |> List.concatMap
            (\from ->
                directionList
                    |> List.concatMap
                        (\to ->
                            Item.smeltable
                                |> List.concatMap
                                    (\i -> rules { from = from, to = to } i)
                        )
            )


pressable : List (Rule CellType)
pressable =
    let
        rules : MovableSort -> { from : Direction, to : Direction } -> ( Item, Item ) -> List (Rule CellType)
        rules movableSort { from, to } ( itemFrom, itemTo ) =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Machine Press True
                        }
              , to =
                    Just
                        { item = Just itemTo
                        , sort = Machine Press False
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (to |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { item = Just itemFrom
                                        , sort = Movable movableSort { from = from, to = to }
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just itemFrom
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set to <|
                                Exactly <|
                                    Just
                                        { item = Nothing
                                        , sort = Machine Press True
                                        }
                           )
              }
            ]
    in
    CellType.movableList
        |> List.concatMap
            (\movableSort ->
                directionList
                    |> List.concatMap
                        (\from ->
                            directionList
                                |> List.concatMap
                                    (\to ->
                                        Item.pressable
                                            |> List.concatMap
                                                (\i -> rules movableSort { from = from, to = to } i)
                                    )
                        )
            )


shreddable : List (Rule CellType)
shreddable =
    let
        rules : MovableSort -> { from : Direction, to : Direction } -> ( Item, Item ) -> List (Rule CellType)
        rules movableSort { from, to } ( itemFrom, itemTo ) =
            List.concat
                [ [ { from =
                        Just
                            { item = Nothing
                            , sort = Machine Shredder True
                            }
                    , to =
                        Just
                            { item = Just itemTo
                            , sort = Machine Shredder True
                            }
                    , neighbors =
                        CellAutomata.anyNeighborhood
                            |> (Neighborhood.set (to |> Direction.flip) <|
                                    Exactly <|
                                        Just <|
                                            { item = Just itemFrom
                                            , sort = Movable movableSort { from = from, to = to }
                                            }
                               )
                    }
                  , { from =
                        Just
                            { item = Just itemTo
                            , sort = Machine Shredder True
                            }
                    , to =
                        Just
                            { item = Nothing
                            , sort = Machine Shredder True
                            }
                    , neighbors =
                        CellAutomata.anyNeighborhood
                    }
                  , { from =
                        Just
                            { item = Just itemFrom
                            , sort = Movable movableSort { from = from, to = to }
                            }
                    , to =
                        Just
                            { item = Nothing
                            , sort = Movable movableSort { from = from, to = to }
                            }
                    , neighbors =
                        CellAutomata.anyNeighborhood
                            |> (Neighborhood.set to <|
                                    Exactly <|
                                        Just
                                            { item = Nothing
                                            , sort = Machine Shredder True
                                            }
                               )
                    }
                  ]
                , if from == to then
                    CellType.containerList
                        |> List.map
                            (\sort ->
                                { from =
                                    Just
                                        { item = Nothing
                                        , sort = sort
                                        }
                                , to =
                                    Just
                                        { item = Just itemTo
                                        , sort = sort
                                        }
                                , neighbors =
                                    CellAutomata.anyNeighborhood
                                        |> (Neighborhood.set to <|
                                                Exactly <|
                                                    Just
                                                        { item = Just itemTo
                                                        , sort = Machine Shredder True
                                                        }
                                           )
                                }
                            )

                  else
                    []
                ]
    in
    CellType.movableList
        |> List.concatMap
            (\movableSort ->
                directionList
                    |> List.concatMap
                        (\from ->
                            directionList
                                |> List.concatMap
                                    (\to ->
                                        Item.shreddable
                                            |> List.concatMap
                                                (\i -> rules movableSort { from = from, to = to } i)
                                    )
                        )
            )


smeltable : List (Rule CellType)
smeltable =
    let
        rules : MovableSort -> { from : Direction, to : Direction } -> ( Item, Item ) -> List (Rule CellType)
        rules movableSort { from, to } ( itemFrom, itemTo ) =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Machine Furnace True
                        }
              , to =
                    Just
                        { item = Just itemTo
                        , sort = Machine Furnace True
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (to |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { item = Just itemFrom
                                        , sort = Movable movableSort { from = from, to = to }
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just itemFrom
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set to <|
                                Exactly <|
                                    Just
                                        { item = Nothing
                                        , sort = Machine Furnace True
                                        }
                           )
              }
            ]
    in
    CellType.movableList
        |> List.concatMap
            (\movableSort ->
                directionList
                    |> List.concatMap
                        (\from ->
                            directionList
                                |> List.concatMap
                                    (\to ->
                                        Item.smeltable
                                            |> List.concatMap
                                                (\i -> rules movableSort { from = from, to = to } i)
                                    )
                        )
            )


burnable : List (Rule CellType)
burnable =
    let
        rules : MachineSort -> MovableSort -> { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules machineSort movableSort { from, to } i =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Machine machineSort False
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Machine machineSort True
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (to |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { item = Just i
                                        , sort = Movable movableSort { from = from, to = to }
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just i
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set to <|
                                Exactly <|
                                    Just
                                        { item = Nothing
                                        , sort = Machine machineSort False
                                        }
                           )
              }
            ]
    in
    CellType.machineList
        |> List.concatMap
            (\machineSort ->
                CellType.movableList
                    |> List.concatMap
                        (\movableSort ->
                            directionList
                                |> List.concatMap
                                    (\from ->
                                        directionList
                                            |> List.concatMap
                                                (\to ->
                                                    Item.burnable
                                                        |> List.concatMap
                                                            (\i -> rules machineSort movableSort { from = from, to = to } i)
                                                )
                                    )
                        )
            )


container : List (Rule CellType)
container =
    let
        rules : MovableSort -> ContainerSort -> { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules movableSort sort { from, to } i =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Just i
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set from <|
                                Exactly <|
                                    Just <|
                                        { sort = sort
                                        , item = Just i
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just i
                        , sort = sort
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = sort
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (from |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { sort = Movable movableSort { from = from, to = to }
                                        , item = Nothing
                                        }
                           )
              }
            ]
    in
    CellType.movableList
        |> List.concatMap
            (\movableSort ->
                CellType.containerList
                    |> List.concatMap
                        (\sort ->
                            directionList
                                |> List.concatMap
                                    (\from ->
                                        directionList
                                            |> List.concatMap
                                                (\to ->
                                                    Item.itemList
                                                        |> List.concatMap
                                                            (\i ->
                                                                rules movableSort sort { from = from, to = to } i
                                                            )
                                                )
                                    )
                        )
            )


movables : List (Rule CellType)
movables =
    let
        rules : MovableSort -> { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules movableSort { from, to } i =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Just i
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> Neighborhood.set from
                            (OneOf
                                (CellType.movableList
                                    |> List.concatMap
                                        (\mov ->
                                            directionList
                                                |> List.map
                                                    (\dir ->
                                                        Just <|
                                                            { sort =
                                                                Movable mov
                                                                    { from = dir
                                                                    , to = from |> Direction.flip
                                                                    }
                                                            , item = Just i
                                                            }
                                                    )
                                        )
                                )
                            )
              }
            , { from =
                    Just
                        { item = Just i
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Movable movableSort { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> Neighborhood.set to
                            (OneOf
                                (CellType.movableList
                                    |> List.concatMap
                                        (\mov ->
                                            directionList
                                                |> List.map
                                                    (\dir ->
                                                        Just <|
                                                            { sort =
                                                                Movable mov
                                                                    { from = to |> Direction.flip
                                                                    , to = dir
                                                                    }
                                                            , item = Nothing
                                                            }
                                                    )
                                        )
                                )
                            )
              }
            ]
    in
    CellType.movableList
        |> List.concatMap
            (\movableSort ->
                directionList
                    |> List.concatMap
                        (\from ->
                            directionList
                                |> List.concatMap
                                    (\to ->
                                        Item.itemList
                                            |> List.concatMap (\i -> rules movableSort { from = from, to = to } i)
                                    )
                        )
            )
