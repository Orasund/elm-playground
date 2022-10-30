module  Data.Item exposing (Item(..), color, description, name, toString)

import Color exposing (Color)
import  View.Color exposing (black, blue, brown)


type Item
    = Water
    | Axe
    | Shit


toString : Item -> String
toString item =
    case item of
        Water ->
            "💧"

        Axe ->
            "\u{1FA93}"

        Shit ->
            "💩"


color : Item -> Color
color cell =
    case cell of
        Water ->
            blue

        Axe ->
            black

        Shit ->
            brown


name : Item -> String
name item =
    case item of
        Water ->
            "Water"

        Axe ->
            "Axe"

        Shit ->
            "Shit"


description : Item -> String
description item =
    case item of
        Water ->
            "Use water to let plants grow into fruits."

        Axe ->
            "Use the axe to knock down trees, chop down wood and even kill animals."

        Shit ->
            "Use shit to speed up the growth of plants."
