module DungeonSokoban.Data.Game.Internal exposing (..)

import Dict exposing (Dict)
import Direction as Direction exposing (Direction(..))
import DungeonSokoban.Config as Config
import DungeonSokoban.Data.Cell exposing (Cell(..))
import Grid exposing (Grid)
import Position
import Random exposing (Generator)
import Random.List
import Set exposing (Set)


type alias Game =
    { board : Grid (Maybe Cell)
    , player : ( Int, Int )
    }


killPlayer : Game -> Game
killPlayer game =
    { game | player = Config.killedPlayer }


updateCell : Direction -> ( ( Int, Int ), Cell ) -> Game -> Game
updateCell defaultDir ( pos, cell ) game =
    case cell of
        Hole ->
            game

        Box ->
            game

        Monster { stunned } ->
            if stunned then
                { game
                    | board =
                        game.board
                            |> Grid.insert pos (Monster { stunned = False })
                }

            else
                let
                    coord =
                        pos
                            |> Position.coordTo game.player

                    newPos =
                        coord
                            |> directionsFromCoord
                            |> List.filter (\dir -> game |> isEmpty dir pos)
                            |> (\list ->
                                    case list of
                                        [ head ] ->
                                            Just head

                                        [] ->
                                            Nothing

                                        _ ->
                                            if list |> List.member defaultDir then
                                                Just defaultDir

                                            else
                                                Nothing
                               )
                            |> Maybe.map
                                (\dir ->
                                    dir
                                        |> Direction.toCoord
                                        |> Position.addTo pos
                                )
                            |> Maybe.withDefault pos
                in
                { game
                    | board =
                        game.board
                            |> Grid.remove pos
                            |> Grid.insert newPos cell
                }
                    |> (if game.player == newPos then
                            killPlayer

                        else
                            identity
                       )


isEmpty : Direction -> ( Int, Int ) -> Game -> Bool
isEmpty dir pos game =
    dir
        |> Direction.toCoord
        |> Position.addTo pos
        |> (\newPos -> Grid.getMember newPos game.board)
        |> (==) Nothing


directionsFromCoord : { x : Int, y : Int } -> List Direction
directionsFromCoord coord =
    (if abs coord.x < abs coord.y then
        []

     else if coord.x > 0 then
        [ Right ]

     else
        [ Left ]
    )
        ++ (if abs coord.y < abs coord.x then
                []

            else if coord.y > 0 then
                [ Down ]

            else
                [ Up ]
           )



-------------------------------------------------------------------------
-- Level


levelFromGrid : Grid (Maybe Cell) -> ( Grid (Maybe Cell), Set ( Int, Int ) )
levelFromGrid grid =
    ( grid
    , grid
        |> Grid.emptyPositions
        |> Set.fromList
    )


randomInsertAll : List Cell -> ( Grid (Maybe Cell), Set ( Int, Int ) ) -> Generator ( Grid (Maybe Cell), Set ( Int, Int ) )
randomInsertAll list ( grid, emptySpaces ) =
    list
        |> List.foldl (\cell -> Random.andThen (randomInsert cell))
            (Random.constant ( grid, emptySpaces ))


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
