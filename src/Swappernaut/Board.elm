module Swappernaut.Board exposing (Board, generator)

import Grid exposing (Grid)
import Random exposing (Generator)
import Random.List as Random
import Swappernaut.Square exposing (Square(..))


type alias Board =
    Grid Square


boardSize : Int
boardSize =
    16


level : Int -> List (List (Maybe Square))
level lv =
    let
        o : Maybe Square
        o =
            Nothing

        g : Maybe Square
        g =
            Just Goal

        w : Maybe Square
        w =
            Just (Wall False)
    in
    [ [ g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
    ]


fixed : Int -> List ( Int, Square )
fixed l =
    []


distribution : Generator (Maybe Square)
distribution =
    Random.weighted
        ( 9, Nothing )
        [ ( 7, Just (Wall False) ), ( 1, Just (Bumper False) ) ]



--------------------------------------------------------------------------------


generator : Int -> Generator Board
generator l =
    let
        distributedBoard : Generator Board
        distributedBoard =
            level l
                |> List.indexedMap (\y -> List.indexedMap (\x square -> ( ( x, y ), square )))
                |> List.concat
                |> List.filterMap
                    (\( loc, maybeSquare ) -> maybeSquare |> Maybe.map (\square -> ( loc, square )))
                |> Grid.fromList
                    { rows = boardSize
                    , columns = boardSize
                    }
                |> (\g ->
                        Random.list (boardSize * boardSize)
                            distribution
                            |> Random.map
                                (List.indexedMap (\i s -> ( ( i |> modBy boardSize, i // boardSize ), s ))
                                    >> List.filterMap (\( pos, maybeS ) -> maybeS |> Maybe.map (\s -> ( pos, s )))
                                    >> List.foldl
                                        (\( pos, square ) ->
                                            Grid.update pos
                                                (Maybe.map Just
                                                    >> Maybe.withDefault (Just <| square)
                                                )
                                        )
                                        g
                                )
                   )
    in
    fixed l
        |> List.map (\( n, s ) -> List.repeat n s)
        |> List.concat
        |> List.foldl
            (\square ->
                Random.map
                    (\( b, positions ) ->
                        case positions of
                            [] ->
                                ( b, positions )

                            pos :: list ->
                                ( b |> Grid.insert pos square, list )
                    )
            )
            (distributedBoard
                |> Random.andThen
                    (\b ->
                        Random.pair distributedBoard
                            (b
                                |> Grid.emptyPositions
                                |> Random.shuffle
                                |> Random.andThen Random.shuffle
                            )
                    )
            )
        |> Random.map Tuple.first
