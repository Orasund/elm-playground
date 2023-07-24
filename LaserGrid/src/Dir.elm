module Dir exposing (..)

import Array exposing (Array)


type alias Dir =
    ( Int, String )


{-|

      3
    2 + 0
      1

-}
new : Int -> Dir
new i =
    ( i, "Dir" )


list : List Dir
list =
    List.range 0 3
        |> List.map new


rotate : Int -> Dir -> Dir
rotate amount ( i, _ ) =
    new (i + amount |> modBy 4)


positions : Array ( Int, Int )
positions =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ] |> Array.fromList


addTo : ( Int, Int ) -> Dir -> ( Int, Int )
addTo ( x2, y2 ) ( i, _ ) =
    let
        ( x1, y1 ) =
            positions |> Array.get i |> Maybe.withDefault ( 0, 0 )
    in
    ( x1 + x2, y1 + y2 )


reverse : Dir -> Dir
reverse =
    rotate 2
