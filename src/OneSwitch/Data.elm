module OneSwitch.Data exposing (boardSize, initialHealth, initialLevel, initialPlayer, screenWidth, spriteSize)

import Grid.Position exposing (Position)


initialLevel : Int
initialLevel =
    1


initialHealth : Int
initialHealth =
    3


initialPlayer : Position
initialPlayer =
    ( boardSize // 2, boardSize // 2 )


spriteSize : Int
spriteSize =
    8


boardSize : Int
boardSize =
    16


screenWidth : Float
screenWidth =
    spriteSize
        * boardSize
        |> toFloat
