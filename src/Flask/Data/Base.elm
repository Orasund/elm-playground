module Flask.Data.Base exposing (Base(..), card, fromString, toString)

import Flask.Data.Effect exposing (Effect(..))
import Flask.Data.Element exposing (Element(..))


type Base
    = Y1
    | Y2
    | R1
    | R2
    | B1
    | B2
    | G1
    | G2


card : Base -> { cost : List Element, discard : Int, effect : Effect }
card base =
    case base of
        Y1 ->
            { cost = [ Red, Blue ], discard = 0, effect = Add [ Yellow, Yellow, Yellow ] }

        Y2 ->
            { cost = [ Yellow, Yellow ], discard = 0, effect = Reboot }

        R1 ->
            { cost = [ Red ], discard = 0, effect = Remove 2 }

        R2 ->
            { cost = [ Red ], discard = 0, effect = Discard 2 }

        B1 ->
            { cost = [], discard = 1, effect = Add [ Blue, Blue ] }

        B2 ->
            { cost = [ Blue ], discard = 0, effect = Draw 1 }

        G1 ->
            { cost = [ Green, Any ], discard = 0, effect = Choose }

        G2 ->
            { cost = [ Green, Green ], discard = 0, effect = Plant }


fromString : String -> Maybe Base
fromString string =
    case string of
        "Y1" ->
            Just Y1

        "Y2" ->
            Just Y2

        "R1" ->
            Just R1

        "R2" ->
            Just R2

        "B1" ->
            Just B1

        "B2" ->
            Just B2

        "G1" ->
            Just G1

        "G2" ->
            Just G2

        _ ->
            Nothing


toString : Base -> String
toString base =
    case base of
        Y1 ->
            "Y1"

        Y2 ->
            "Y2"

        R1 ->
            "R1"

        R2 ->
            "R2"

        B1 ->
            "B1"

        B2 ->
            "B2"

        G1 ->
            "G1"

        G2 ->
            "G2"
