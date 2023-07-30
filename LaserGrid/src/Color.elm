module Color exposing (..)


background level =
    "linear-gradient(" ++ white ++ ", " ++ inactiveLaser level ++ ")"


fontColor =
    black


wallColor =
    black


laserColor level =
    case level of
        2 ->
            primaryLevel2

        1 ->
            primaryLevel1

        _ ->
            Debug.todo "add Laser color"


laserColorLevel1 =
    primaryLevel1


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


secondary : String
secondary =
    --dark blue
    "#122a58"
