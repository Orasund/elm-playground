module Data.Position exposing (..)


neighbors : ( Int, Int ) -> List ( Int, Int )
neighbors ( x, y ) =
    [ ( x, y + 1 ), ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ) ]
