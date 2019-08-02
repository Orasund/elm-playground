module GameJam.Data.Behaviour exposing (activate, consumable, removeToWin)

import GameJam.Data.Board as Board exposing (Board)
import GameJam.Data.Square as Square exposing (Square(..))
import Grid exposing (Grid)
import Grid.Position as Position exposing (Position)


removeToWin : List Square
removeToWin =
    [ Enemy, InactiveEnemy, OpenDoor, LookedDoor ]


consumable : Bool -> List Square
consumable super =
    [ Health, OpenDoor ]
        ++ (if super then
                [ Enemy ]

            else
                []
           )


activate : Square -> Board -> Board
activate square =
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
