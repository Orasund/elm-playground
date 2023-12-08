module Location exposing (..)

import Card exposing (Card(..))


type Location
    = TownSquare
    | OldLady
    | Cemetery
    | Crypt


name : Location -> String
name location =
    case location of
        TownSquare ->
            "auf dem Dorfplatz"

        OldLady ->
            "im Wohnzimmer einer alten Dame"

        Cemetery ->
            "auf dem Friedhof"

        Crypt ->
            "im der Gruft"
