module GameJam.Data.Board exposing (Board, generator)

import GameJam.Data exposing (boardSize)
import GameJam.Data.Square exposing (Square(..))
import Grid exposing (Grid)
import Random exposing (Generator)
import Random.List as Random


type alias Board =
    Grid Square


level : Int -> List (List (Maybe Square))
level lv =
    let
        w : Maybe Square
        w =
            Just Wall

        o : Maybe Square
        o =
            Nothing

        l : Maybe Square
        l =
            Just LookedDoor

        d : Maybe Square
        d =
            Just OpenDoor

        m : Maybe Square
        m =
            Just Lava

        s : Maybe Square
        s =
            Just Swap

        p : Maybe Square
        p =
            Just PowerUp

        k : Maybe Square
        k =
            Just Key
    in
    case lv of
        1 ->
            [ [ w, w, w, w, w, w, w, w, m, w, w, w, w, w, w, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, s, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, w, w, w, w, w, w, m, w, w, w, w, w, w, w, w ]
            ]

        2 ->
            [ [ w, w, w, w, w, w, w, w, m, w, w, w, w, w, w, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, d, s, k, d, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, w, w, w, w, w, w, m, w, w, w, w, w, w, w, w ]
            ]

        3 ->
            [ [ w, w, w, w, w, w, w, w, m, w, w, w, w, w, w, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, d, m, w, l, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, w, s, o, m, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, d, m, w, l, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, w, w, w, w, w, w, m, w, w, w, w, w, w, w, w ]
            ]

        4 ->
            [ [ w, w, w, w, w, w, w, w, m, w, w, w, w, w, w, w ]
            , [ w, o, o, o, o, o, o, d, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, w, o, w, m, w, o, p, m, w, m, o, m, o, w ]
            , [ w, d, o, w, o, w, m, s, o, w, m, o, m, o, l, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, l, o, o, o, o, o, o, w ]
            , [ w, w, w, w, w, w, w, m, w, w, w, w, w, w, w, w ]
            ]

        5 ->
            [ [ w, w, w, w, w, w, w, w, m, w, w, w, w, w, w, w ]
            , [ w, o, o, o, w, o, o, d, o, o, o, o, o, o, o, w ]
            , [ w, o, o, w, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, w, o, o, m, w, m, o, m, o, m, o, w ]
            , [ w, o, o, w, o, o, o, w, m, o, m, o, m, o, m, w ]
            , [ w, o, o, o, w, o, o, m, w, o, o, o, o, o, o, w ]
            , [ w, o, o, w, o, o, o, w, m, o, o, o, o, o, o, w ]
            , [ w, o, w, o, w, m, w, o, o, m, w, m, o, m, o, w ]
            , [ w, d, o, w, o, w, m, s, o, w, m, o, m, o, l, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, w, o, o, w ]
            , [ w, o, o, o, o, o, o, w, m, o, o, w, o, o, o, w ]
            , [ w, m, o, m, o, m, o, m, w, o, o, o, w, o, o, w ]
            , [ w, o, m, o, m, o, m, w, m, o, o, w, o, o, o, w ]
            , [ w, o, o, o, o, o, o, m, w, o, o, o, w, o, o, w ]
            , [ w, o, o, o, o, o, o, o, l, o, o, w, o, o, o, w ]
            , [ w, w, w, w, w, w, w, m, w, w, w, w, w, w, w, w ]
            ]

        _ ->
            level 5


fixed : Int -> List ( Int, Square )
fixed l =
    case l of
        1 ->
            [ ( 4, Enemy ), ( 2, Health ) ]

        2 ->
            [ ( 4, Enemy ), ( 2, Health ), ( 1, Key ) ]

        3 ->
            [ ( 3, Enemy ), ( 2, Health ), ( 1, Key ) ]

        4 ->
            [ ( 3, Enemy ), ( 2, Health ), ( 1, Key ) ]

        _ ->
            [ ( l, Enemy ), ( l, Health ), ( 1, Key ), ( 1, Weapon ), ( 1, PowerUp ) ]


distribution : Generator (Maybe Square)
distribution =
    Random.weighted
        ( 1, Nothing )
        []



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
