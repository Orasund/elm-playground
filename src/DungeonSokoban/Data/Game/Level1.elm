module DungeonSokoban.Data.Game.Level1 exposing (..)

import DungeonSokoban.Data.Cell exposing (Cell(..))
import Grid exposing (Grid)
import Random exposing (Generator)
import Set exposing (Set)


generate : Generator (Grid (Maybe Cell))
generate =
    let
        grid =
            [ [ Just Box, Nothing, Nothing, Just Box ]
            , [ Nothing, Nothing, Nothing, Nothing ]
            , [ Nothing, Nothing, Nothing, Nothing ]
            , [ Just Box, Nothing, Nothing, Just Box ]
            ]
                |> Grid.build

        emptySpaces =
            grid
                |> Grid.emptyPositions
                |> Set.fromList
    in
    ( grid, emptySpaces )
        |> randomInsert Hole
        |> Random.andThen (randomInsert (Monster { stunned = False }))
        |> Random.andThen (randomInsert Box)
        |> Random.map Tuple.first


randomInsert : Cell -> ( Grid (Maybe Cell), Set ( Int, Int ) ) -> Generator ( Grid (Maybe Cell), Set ( Int, Int ) )
randomInsert elem ( grid, emptySpaces ) =
    case emptySpaces |> Set.toList of
        [] ->
            Random.constant ( grid, emptySpaces )

        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\pos ->
                        ( grid |> Grid.insert pos elem
                        , emptySpaces |> Set.remove pos
                        )
                    )
