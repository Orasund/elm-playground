module Game exposing (..)

import Config
import Dict exposing (Dict)
import Gen.Enum.Natural exposing (Natural(..))
import Insect exposing (Action(..), Insect(..))
import Process exposing (spawn)
import Random exposing (Generator)


type Tile
    = NaturalTile Natural
    | InsectTile Insect


type alias Game =
    { grid : Dict ( Int, Int ) Tile
    , next : Natural
    }


new : Game
new =
    { grid = Dict.empty
    , next = Leaf
    }


place : Int -> Game -> Maybe (Generator Game)
place x game =
    case Dict.get ( x, 0 ) game.grid of
        Just _ ->
            Nothing

        Nothing ->
            Gen.Enum.Natural.asList
                |> (\list ->
                        case list of
                            head :: tail ->
                                ( head, tail )

                            [] ->
                                ( game.next, [] )
                   )
                |> (\( head, tail ) -> Random.uniform head tail)
                |> Random.map
                    (\next ->
                        { game
                            | grid =
                                game.grid
                                    |> Dict.insert ( x, 0 ) (NaturalTile game.next)
                            , next = next
                        }
                    )
                |> Just



---


fallTile : ( Int, Int ) -> Dict ( Int, Int ) Tile -> Maybe (Dict ( Int, Int ) Tile)
fallTile ( x, y ) grid =
    case grid |> Dict.get ( x, y ) of
        Just tile ->
            if y + 1 < Config.rows then
                case grid |> Dict.get ( x, y + 1 ) of
                    Just below ->
                        Nothing

                    Nothing ->
                        grid
                            |> Dict.insert ( x, y + 1 ) tile
                            |> Dict.remove ( x, y )
                            |> Just

            else
                Nothing

        Nothing ->
            Nothing


fall : Game -> Maybe Game
fall game =
    List.range 0 (Config.rows - 1)
        |> List.foldl
            (\row grid ->
                let
                    y =
                        Config.rows - 1 - row
                in
                List.range 0 (Config.columns - 1)
                    |> List.foldl
                        (\x maybe ->
                            maybe
                                |> Maybe.withDefault game.grid
                                |> fallTile ( x, y )
                                |> Maybe.map Just
                                |> Maybe.withDefault maybe
                        )
                        grid
            )
            Nothing
        |> Maybe.map (\grid -> { game | grid = grid })



-----


matchList : Tile -> List ( Int, Int ) -> { read : Dict ( Int, Int ) Tile, write : Dict ( Int, Int ) Tile } -> Maybe (Dict ( Int, Int ) Tile)
matchList tile list { read, write } =
    if
        list
            |> List.all (\pos -> Dict.get pos read == Just tile)
    then
        list
            |> List.foldl Dict.remove write
            |> Just

    else
        Nothing


spawnInsect : Tile -> Maybe Insect
spawnInsect tile =
    case tile of
        NaturalTile natural ->
            Insect.spawn natural
                |> Just

        _ ->
            Nothing


match : Game -> Maybe Game
match game =
    game.grid
        |> Dict.foldl
            (\( x, y ) tile maybe ->
                [ [ ( x, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                , [ ( x, y ), ( x - 1, y ), ( x + 1, y ) ]
                , [ ( x, y ), ( x - 1, y - 1 ), ( x + 1, y + 1 ) ]
                , [ ( x, y ), ( x - 1, y + 1 ), ( x + 1, y - 1 ) ]
                ]
                    |> List.foldl
                        (\list grid ->
                            grid
                                |> Maybe.withDefault game.grid
                                |> (\write -> { read = game.grid, write = write })
                                |> matchList tile list
                                |> Maybe.map
                                    (\g ->
                                        tile
                                            |> spawnInsect
                                            |> Maybe.map
                                                (\insect ->
                                                    g
                                                        |> Dict.insert ( x, y ) (InsectTile insect)
                                                )
                                            |> Maybe.withDefault g
                                            |> Just
                                    )
                                |> Maybe.withDefault grid
                        )
                        maybe
            )
            Nothing
        |> Maybe.map (\grid -> { game | grid = grid })



---


moveInsect : ( Int, Int ) -> Insect -> Dict ( Int, Int ) Tile -> Generator (Dict ( Int, Int ) Tile)
moveInsect ( x, y ) insect dict =
    let
        vec ( a, b ) =
            { x = a, y = b }
    in
    [ vec ( x - 1, y )
    , vec ( x + 1, y )
    , vec ( x, y - 1 )
    , vec ( x - 1, y - 1 )
    , vec ( x + 1, y - 1 )
    , vec ( x, y + 1 )
    , vec ( x - 1, y + 1 )
    , vec ( x + 1, y + 1 )
    ]
        |> List.filterMap
            (\pos ->
                if (0 <= pos.x) && (pos.x < Config.columns) && (0 <= pos.y) && (pos.y < Config.rows) then
                    (case Dict.get ( pos.x, pos.y ) dict of
                        Just (NaturalTile natural) ->
                            Just (Just natural)

                        Nothing ->
                            Just Nothing

                        _ ->
                            Nothing
                    )
                        |> Maybe.andThen
                            (\maybe ->
                                if Insect.movement ( x, y ) ( ( pos.x, pos.y ), maybe ) insect then
                                    Just ( pos, maybe )

                                else
                                    Nothing
                            )

                else
                    Nothing
            )
        |> (\list ->
                case list of
                    head :: tail ->
                        Random.uniform head tail
                            |> Random.map
                                (\( pos, maybe ) ->
                                    case Insect.move maybe insect of
                                        Just MoveTo ->
                                            dict
                                                |> Dict.insert ( pos.x, pos.y ) (InsectTile insect)
                                                |> Dict.remove ( x, y )

                                        Just (SwapTo natural) ->
                                            dict
                                                |> Dict.insert ( pos.x, pos.y ) (InsectTile insect)
                                                |> Dict.insert ( x, y ) (NaturalTile natural)

                                        Nothing ->
                                            dict
                                )

                    [] ->
                        Random.constant dict
           )


move : Game -> Generator Game
move game =
    game.grid
        |> Dict.toList
        |> List.filterMap
            (\( pos, tile ) ->
                case tile of
                    InsectTile insect ->
                        Just ( pos, insect )

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\( pos, insect ) ->
                Random.andThen (moveInsect pos insect)
            )
            (Random.constant game.grid)
        |> Random.map (\grid -> { game | grid = grid })
