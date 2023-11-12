module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game
import Set
import Test exposing (..)


suite : Test
suite =
    let
        tiles =
            [ -1, 1, 2, 3, 4, 5, 6 ]
    in
    (\() ->
        { board =
            [ ( ( 0, 0 ), 1 )
            , ( ( 0, 1 ), 2 )
            , ( ( 1, 0 ), 3 )
            , ( ( 1, 1 ), 3 )
            , ( ( 0, 2 ), 4 )
            , ( ( 0, 3 ), 4 )
            , ( ( 1, 2 ), 5 )
            , ( ( 1, 3 ), 6 )
            , ( ( 2, 3 ), -1 )
            ]
                |> Dict.fromList
        , width = 3
        , height = 4
        , goal = ( 2, -1 )
        , tiles = tiles
        }
            |> Game.findSoftlocks
            |> Expect.equalSets Set.empty
    )
        |> Test.test "No softlocks"
