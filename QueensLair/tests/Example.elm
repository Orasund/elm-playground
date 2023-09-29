module Example exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game
import Piece exposing (Piece(..))
import Test exposing (..)


suite : Test
suite =
    Test.describe "basic moves"
        [ Test.test "one"
            (\() ->
                [ ( ( 2, 0 ), { isWhite = False, piece = Bishop } )
                , ( ( 0, 3 ), { isWhite = True, piece = King } )
                , ( ( 3, 2 ), { isWhite = False, piece = Bishop } )
                , ( ( 3, 1 ), { isWhite = False, piece = Pawn } )
                ]
                    |> Dict.fromList
                    |> Game.fromBoard
                    |> Game.findNextMove
                    |> Expect.equal (Just { from = ( 3, 2 ), to = ( 2, 1 ) })
            )
        ]
