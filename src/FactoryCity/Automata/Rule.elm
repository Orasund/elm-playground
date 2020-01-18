module FactoryCity.Automata.Rule exposing (burnable, container, movables, smeltable)

import CellAutomata exposing (Rule, RuleExpression(..))
import FactoryCity.Automata.Neighborhood as Neighborhood
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..), Item)
import Grid.Direction as Direction exposing (Direction(..))


directionList : List Direction
directionList =
    [ Up, Right, Down, Left ]


smeltable : List (Rule CellType)
smeltable =
    let
        rules : { from : Direction, to : Direction } -> ( Item, Item ) -> List (Rule CellType)
        rules { from, to } ( itemFrom, itemTo ) =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Furnace { isWarm = True }
                        }
              , to =
                    Just
                        { item = Just itemTo
                        , sort = Furnace { isWarm = True }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (to |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { item = Just itemFrom
                                        , sort = Belt { from = from, to = to }
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just itemFrom
                        , sort = Belt { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Belt { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set to <|
                                Exactly <|
                                    Just
                                        { item = Nothing
                                        , sort = Furnace { isWarm = True }
                                        }
                           )
              }
            ]
    in
    directionList
        |> List.concatMap
            (\from ->
                directionList
                    |> List.concatMap
                        (\to ->
                            CellType.smeltable
                                |> List.concatMap (\i -> rules { from = from, to = to } i)
                        )
            )


burnable : List (Rule CellType)
burnable =
    let
        rules : { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules { from, to } i =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Furnace { isWarm = False }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Furnace { isWarm = True }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set (to |> Direction.flip) <|
                                Exactly <|
                                    Just <|
                                        { item = Just i
                                        , sort = Belt { from = from, to = to }
                                        }
                           )
              }
            , { from =
                    Just
                        { item = Just i
                        , sort = Belt { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Belt { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> (Neighborhood.set to <|
                                Exactly <|
                                    Just
                                        { item = Nothing
                                        , sort = Furnace { isWarm = False }
                                        }
                           )
              }
            ]
    in
    directionList
        |> List.concatMap
            (\from ->
                directionList
                    |> List.concatMap
                        (\to ->
                            CellType.burnable
                                |> List.concatMap (\i -> rules { from = from, to = to } i)
                        )
            )


container : List (Rule CellType)
container =
    let
        rules : ContainerSort -> { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules sort { from, to } i =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Belt { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Just i
                        , sort = Belt { from = from, to = to }
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
                                        { sort = Belt { from = from, to = to }
                                        , item = Nothing
                                        }
                           )
              }
            ]
    in
    CellType.containerList
        |> List.concatMap
            (\sort ->
                directionList
                    |> List.concatMap
                        (\from ->
                            directionList
                                |> List.concatMap
                                    (\to ->
                                        CellType.itemList
                                            |> List.concatMap (\i -> rules sort { from = from, to = to } i)
                                    )
                        )
            )


movables : List (Rule CellType)
movables =
    let
        rules : { from : Direction, to : Direction } -> Item -> List (Rule CellType)
        rules { from, to } i =
            [ { from =
                    Just
                        { item = Nothing
                        , sort = Belt { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Just i
                        , sort = Belt { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> Neighborhood.set from
                            (OneOf
                                (directionList
                                    |> List.map
                                        (\dir ->
                                            Just <|
                                                { sort =
                                                    Belt
                                                        { from = dir
                                                        , to = from |> Direction.flip
                                                        }
                                                , item = Just i
                                                }
                                        )
                                )
                            )
              }
            , { from =
                    Just
                        { item = Just i
                        , sort = Belt { from = from, to = to }
                        }
              , to =
                    Just
                        { item = Nothing
                        , sort = Belt { from = from, to = to }
                        }
              , neighbors =
                    CellAutomata.anyNeighborhood
                        |> Neighborhood.set to
                            (OneOf
                                (directionList
                                    |> List.map
                                        (\dir ->
                                            Just <|
                                                { sort =
                                                    Belt
                                                        { from = to |> Direction.flip
                                                        , to = dir
                                                        }
                                                , item = Nothing
                                                }
                                        )
                                )
                            )
              }
            ]
    in
    directionList
        |> List.concatMap
            (\from ->
                directionList
                    |> List.concatMap
                        (\to ->
                            CellType.itemList
                                |> List.concatMap (\i -> rules { from = from, to = to } i)
                        )
            )
