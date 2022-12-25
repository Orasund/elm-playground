module Config exposing (..)

import Data.Zoom exposing (Zoom)


width : Zoom -> Int
width zoom =
    21 * Data.Zoom.get zoom |> round


height : Zoom -> Int
height zoom =
    15 * Data.Zoom.get zoom |> round


tileSize zoom =
    "min("
        ++ ("100vw/" ++ String.fromInt (width zoom) ++ ", ")
        ++ ("100vh/" ++ String.fromInt (height zoom))
        ++ ")"


fontSize size zoom =
    "min("
        ++ ((100 * size |> String.fromFloat) ++ "vw/" ++ String.fromInt (width zoom) ++ ", ")
        ++ ((100 * size |> String.fromFloat) ++ "vh/" ++ String.fromInt (height zoom))
        ++ ")"


hqPos =
    ( 0, 0 )


maxCameraDistance z w h =
    toFloat (min (w z) (h z))
        / 4
        |> round


wagonCost =
    8


trainLoadSize =
    wagonMaxItems


tracksPerTrip =
    8


maxLevel =
    9


wagonMaxItems =
    10


trackCost =
    1


bombCost =
    2


bombExplosionTime =
    10
