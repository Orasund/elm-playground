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

        e : Maybe Square
        e =
            Just Enemy

        m : Maybe Square
        m =
            Just Lava

        s : Maybe Square
        s =
            Just Swap
    in
    case lv of
        1 ->
            [ [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, m, o, o, o, o, o, o, w ]
            , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
            ]

        2 ->
            [ [ w, w, w, w, w, w, w, d, w, w, w, w, w, w, w, w ]
            , [ w, o, o, o, o, o, w, s, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, w, l, w, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w ]
            , [ w, w, w, w, w, w, w, w, w, w, w, w, w, w, w, w ]
            ]

        _ ->
            level 1


fixed : List ( Int, Square )
fixed =
    [ ( 3, Enemy )
    , ( 1, OpenDoor )
    , ( 1, LookedDoor )
    , ( 1, InactiveEnemy )
    , ( 1, Key )
    , ( 1, PowerUp )
    , ( 1, PowerDown )
    , ( 1, Weapon )
    , ( 1, Swap )
    , ( 1, Lava )
    ]


distribution : Generator (Maybe Square)
distribution =
    Random.weighted
        ( 1, Nothing )
        [ ( 0.005, Just Health ) ]



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
    fixed
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
