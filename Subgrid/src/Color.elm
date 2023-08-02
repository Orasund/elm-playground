module Color exposing (..)

import Level exposing (Level, LevelAmount)
import StaticArray exposing (StaticArray)


background : Level -> String
background level =
    "linear-gradient(" ++ white ++ ", " ++ inactiveLaser level ++ ")"


fontColor : String
fontColor =
    black


wallColor : String
wallColor =
    black


laserColor : Level -> Int -> String
laserColor level _ =
    "color-mix(in lch,"
        ++ (primaryColors |> StaticArray.get level)
        ++ ", white "
        ++ String.fromInt 0
        --(amount * 20)
        ++ "%)"


inactiveLaser : Level -> String
inactiveLaser level =
    "color-mix(in lch," ++ laserColor level 0 ++ " 33%,white)"



----------------------------------------------------------------------
--
----------------------------------------------------------------------


black : String
black =
    secondary


white : String
white =
    "#f6feff"


lightGray : String
lightGray =
    "#f2f3f3"


darkGray : String
darkGray =
    "#d9d9d9"


primaryColors : StaticArray LevelAmount String
primaryColors =
    ( --red
      "#cc353c"
    , [ --green
        "#23bf24"
      , --violett
        "#cc35a1"
      ]
    )
        |> StaticArray.fromList Level.maxLevel


secondary : String
secondary =
    --dark blue
    "#122a58"
