module Config exposing (..)


width =
    21


height =
    15


tileSize =
    "min(32px, "
        ++ (String.fromFloat (100 / toFloat width) ++ "vw, ")
        ++ ("(100vh - 50px)/" ++ String.fromInt height)
        ++ ")"


hqPos =
    ( width // 2, 0 )


maxCameraDistance =
    5


wagonCost =
    8


tracksPerTrip =
    8


wagonMaxItems =
    20


trackCost =
    1
