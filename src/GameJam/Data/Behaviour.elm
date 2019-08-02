module GameJam.Data.Behaviour exposing (activate, consumable, removeToWin)

import GameJam.Data.Board exposing (Board)
import GameJam.Data.Square as Square exposing (Square(..))
import Grid


removeToWin : Int -> List Square
removeToWin _ =
    [ Enemy, InactiveEnemy, OpenDoor, LookedDoor ]


consumable : Int -> Bool -> List Square
consumable _ super =
    [ Health, OpenDoor ]
        ++ (if super then
                [ Enemy ]

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
        (\position ->
            Maybe.map
                (\square ->
                    if square |> fun then
                        square |> Square.swap

                    else
                        square
                )
        )
