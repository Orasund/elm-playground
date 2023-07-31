module Color exposing (..)

import Level exposing (Level(..))


background : Level -> String
background level =
    "linear-gradient(" ++ white ++ ", " ++ inactiveLaser level ++ ")"


fontColor : String
fontColor =
    black


wallColor : String
wallColor =
    black


laserColor : Level -> String
laserColor level =
    case level of
        Level2 ->
            primaryLevel2

        Level1 ->
            primaryLevel1


inactiveLaser : Level -> String
inactiveLaser level =
    "color-mix(in lch," ++ laserColor level ++ " 33%,white)"



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


primaryLevel1 : String
primaryLevel1 =
    --red
    "#cc353c"


primaryLevel2 : String
primaryLevel2 =
    --green
    "#23bf24"


primaryLevel3 : String
primaryLevel3 =
    --violett
    "#cc35a1"


secondary : String
secondary =
    --dark blue
    "#122a58"
