module OneSwitch.Data.Behaviour exposing (activate, consumable, removeToWin)

import OneSwitch.Data.Board
import OneSwitch.Data.Square as Square exposing (Square(..))
import Grid exposing (Grid)


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


activate : Int -> Square -> Grid Square -> Grid Square
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


ifThenSwap : (Square -> Bool) -> Grid Square -> Grid Square
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
