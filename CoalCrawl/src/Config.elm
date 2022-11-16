module Config exposing (..)


width =
    21


height =
    15


tileSize =
    "min("
        ++ ("100vw/" ++ String.fromInt width ++ ", ")
        ++ ("100vh/" ++ String.fromInt height)
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
    10


trackCost =
    1


bombCost =
    1


bombExplosionTime =
    10
