module Swappernaut.Square exposing (Square(..), view)

import PixelEngine.Tile as Tile exposing (Tile)


type Square
    = Wall Bool
    | Goal
    | Bumper Bool


view : Square -> Tile Never
view square =
    Tile.fromPosition <|
        case square of
            Wall focus ->
                if focus then ( 0, 0 ) else (2,0)

            Goal ->
                ( 1, 0 )

            Bumper focus ->
                if focus then ( 1, 1 ) else (3,0)
