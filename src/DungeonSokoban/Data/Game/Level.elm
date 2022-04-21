module DungeonSokoban.Data.Game.Level exposing (..)

import DungeonSokoban.Data.Cell as Cell exposing (Cell(..))
import DungeonSokoban.Data.Game.Internal as Internal
import Grid exposing (Grid)
import Random exposing (Generator)
import Set exposing (Set)


level6 : Generator (Grid (Maybe Cell))
level6 =
    [ [ Just Box, Just Box, Just Box, Just Box ]
    , [ Nothing, Nothing, Just Hole, Just Box ]
    , [ Nothing, Just Box, Nothing, Just Box ]
    , [ Just Box, Nothing, Nothing, Just Box ]
    ]
        |> Grid.build
        |> Internal.levelFromGrid
        |> Internal.randomInsertAll
            (List.concat
                [ List.repeat 1 Cell.monster
                , List.repeat 0 Box
                ]
            )
        |> Random.map Tuple.first


level5 : Generator (Grid (Maybe Cell))
level5 =
    [ [ Just Hole, Just Box, Just Box, Just Box ]
    , [ Nothing, Nothing, Just Cell.monster, Just Box ]
    , [ Nothing, Just Box, Nothing, Just Box ]
    , [ Just Box, Nothing, Nothing, Just Hole ]
    ]
        |> Grid.build
        |> Internal.levelFromGrid
        |> Internal.randomInsertAll
            (List.concat
                [ List.repeat 1 Cell.monster
                , List.repeat 0 Box
                ]
            )
        |> Random.map Tuple.first


level4 : Generator (Grid (Maybe Cell))
level4 =
    [ [ Just Hole, Just Hole, Just Hole, Just Hole ]
    , [ Nothing, Nothing, Just Box, Just Hole ]
    , [ Just Box, Nothing, Nothing, Just Hole ]
    , [ Just Box, Nothing, Nothing, Just Hole ]
    ]
        |> Grid.build
        |> Internal.levelFromGrid
        |> Internal.randomInsertAll
            (List.concat
                [ List.repeat 2 Cell.monster
                , List.repeat 0 Box
                ]
            )
        |> Random.map Tuple.first


level3 : Generator (Grid (Maybe Cell))
level3 =
    [ [ Just Cell.monster, Nothing, Nothing, Nothing ]
    , [ Nothing, Nothing, Just Hole, Nothing ]
    , [ Nothing, Nothing, Nothing, Nothing ]
    , [ Just Box, Nothing, Nothing, Just Cell.monster ]
    ]
        |> Grid.build
        |> Internal.levelFromGrid
        |> Internal.randomInsert Box
        |> Random.map Tuple.first


level2 : Generator (Grid (Maybe Cell))
level2 =
    [ [ Just Hole, Nothing, Just Cell.monster, Nothing ]
    , [ Nothing, Nothing, Nothing, Just Cell.monster ]
    , [ Nothing, Nothing, Nothing, Nothing ]
    , [ Just Box, Nothing, Nothing, Just Hole ]
    ]
        |> Grid.build
        |> Internal.levelFromGrid
        |> Internal.randomInsertAll (List.repeat 2 Box)
        |> Random.map Tuple.first


level1 : Generator (Grid (Maybe Cell))
level1 =
    [ [ Just Box, Nothing, Nothing, Just Box ]
    , [ Nothing, Nothing, Nothing, Nothing ]
    , [ Nothing, Just Hole, Nothing, Nothing ]
    , [ Just Box, Nothing, Nothing, Just Box ]
    ]
        |> Grid.build
        |> Internal.levelFromGrid
        |> Internal.randomInsertAll [ Cell.monster, Box ]
        |> Random.map Tuple.first
