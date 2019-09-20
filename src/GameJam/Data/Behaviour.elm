module GameJam.Data.Behaviour exposing (activate, consumable, removeToWin)

import GameJam.Data.Board exposing (Board)
import GameJam.Data.Square as Square exposing (Square(..))
import Grid


removeToWin : Int -> List Square
removeToWin _ =
    [ Enemy, InactiveEnemy ]


consumable : Int -> Bool -> List Square
consumable _ super =
    [ Health, Key, OpenDoor ]
        ++ (if super then
                [ InactiveEnemy ]

            else
                []
           )


activate : Int -> Square -> Board -> Board
activate _ square =
    case square of
        Swap ->
            ifThenSwap <|
                always True

        Weapon ->
            ifThenSwap <|
                \s -> s == Enemy

        Key ->
            ifThenSwap <|
                \s -> s == LookedDoor

        _ ->
            identity



--------------------------------------------------------------------------------


ifThenSwap : (Square -> Bool) -> Board -> Board
ifThenSwap fun =
    Grid.map
        (always <|
            Maybe.map
                (\square ->
                    if square |> fun then
                        square |> Square.swap

                    else
                        square
                )
        )
