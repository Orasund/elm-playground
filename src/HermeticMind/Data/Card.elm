module HermeticMind.Data.Card exposing (Card(..), color, description, title, value)

import HermeticMind.View.Color as Color


type Card
    = Binary Int
    | Trump Int
    | Element Int
    | Planet Int
    | Virtue Int
    | Joker
    | Back


color : Card -> String
color card =
    case card of
        Binary 1 ->
            "white"

        Binary 0 ->
            "black"

        Trump n ->
            Color.gray

        Planet _ ->
            "black"

        Element _ ->
            "black"

        Virtue n ->
            if n <= 4 then
                Color.blue

            else if n <= 8 then
                Color.red

            else if n <= 12 then
                Color.green

            else
                Color.yellow

        _ ->
            "none"


title : Card -> String
title card =
    case card of
        Joker ->
            "Narr"

        Binary n ->
            case n of
                0 ->
                    "Anfang"

                1 ->
                    "Ende"

                _ ->
                    "Binary"

        Trump n ->
            case n of
                1 ->
                    "Der Magier"

                2 ->
                    "Die Hohepriesterin"

                3 ->
                    "Die Herrscherin"

                4 ->
                    "Der Herrscher"

                5 ->
                    "Der Hierophant"

                6 ->
                    "Die Liebenden"

                7 ->
                    "Der Wagen"

                8 ->
                    "Die Gerechtigkeit"

                9 ->
                    "Der Eremit"

                10 ->
                    "Das Rad des Schicksals"

                11 ->
                    "Die Kraft"

                12 ->
                    "Der Gehängte"

                13 ->
                    "Der Tod"

                14 ->
                    "Die Mäßigkeit"

                15 ->
                    "Der Teufel"

                16 ->
                    "Der Turm"

                17 ->
                    "Der Stern"

                18 ->
                    "Der Mond"

                19 ->
                    "Die Sonne"

                20 ->
                    "Das Gericht"

                21 ->
                    "Die Welt"

                _ ->
                    "Trumpf"

        Element n ->
            case n of
                1 ->
                    "Erde"

                2 ->
                    "Feuer"

                3 ->
                    "Luft"

                4 ->
                    "Wasser"

                _ ->
                    "Elemente"

        Planet n ->
            case n of
                1 ->
                    "Merkur"

                2 ->
                    "Venus"

                3 ->
                    "Planet Erde"

                4 ->
                    "Mars"

                5 ->
                    "Jupiter"

                6 ->
                    "Saturn"

                7 ->
                    "Uranus"

                8 ->
                    "Neptun"

                _ ->
                    "Planet"

        Virtue n ->
            case n of
                1 ->
                    "Mitgefühl"

                2 ->
                    "Freundlichkeit"

                3 ->
                    "Offenheit"

                4 ->
                    "Vergebung"

                5 ->
                    "Geduld"

                6 ->
                    "Treue"

                7 ->
                    "Selbstbeherrschung"

                8 ->
                    "Ausdauer"

                9 ->
                    "Selbsterkenntnis"

                10 ->
                    "Autentizität"

                11 ->
                    "Ehrlichkeit"

                12 ->
                    "Mäßigkeit"

                13 ->
                    "Humor"

                14 ->
                    "Hoffnung"

                15 ->
                    "Mut"

                16 ->
                    "Fleiß"

                _ ->
                    "Tugend"

        Back ->
            ""


description : Card -> String
description card =
    case card of
        Joker ->
            "Sorglosigkeit"

        Binary n ->
            case n of
                0 ->
                    "Geborgenheit"

                1 ->
                    "Wandel"

                _ ->
                    "Binary"

        Trump n ->
            case n of
                1 ->
                    "Klarheit"

                2 ->
                    "Intuition"

                3 ->
                    "Selbstvertrauen"

                4 ->
                    "Selbstbeherrschung"

                5 ->
                    "Spiritualität"

                6 ->
                    "Verbindung"

                7 ->
                    "Erfolg"

                8 ->
                    "Gerechtigkeit"

                9 ->
                    "Suche"

                10 ->
                    "Schicksal"

                11 ->
                    "Kraft"

                12 ->
                    "Ruhe"

                13 ->
                    "Wandel"

                14 ->
                    "Mäßigkeit"

                15 ->
                    "Abhängigkeit"

                16 ->
                    "Konfrontation"

                17 ->
                    "Hoffnung"

                18 ->
                    "Hingabe"

                19 ->
                    "Zufriedenheit"

                20 ->
                    "Neubeginn"

                21 ->
                    "Selbsterkenntnis"

                _ ->
                    "Trumpf"

        Element n ->
            case n of
                1 ->
                    "Traum"

                2 ->
                    "Emotion"

                3 ->
                    "Realität"

                4 ->
                    "Taktik"

                _ ->
                    "Jahreszeit"

        Planet n ->
            case n of
                1 ->
                    "Wissen"

                2 ->
                    "Familie"

                3 ->
                    "Gesundheit"

                4 ->
                    "Reichtum"

                5 ->
                    "Verwirklichung"

                6 ->
                    "Anerkennung"

                7 ->
                    "Erfolg"

                8 ->
                    "Freiheit"

                _ ->
                    "Jahreszeit"

        Virtue n ->
            case n of
                1 ->
                    "Mitgefühl"

                2 ->
                    "Freundlichkeit"

                3 ->
                    "Offenheit"

                4 ->
                    "Vergebung"

                5 ->
                    "Geduld"

                6 ->
                    "Treue"

                7 ->
                    "Selbstbeherrschung"

                8 ->
                    "Ausdauer"

                9 ->
                    "Selbsterkenntnis"

                10 ->
                    "Autentizität"

                11 ->
                    "Ehrlichkeit"

                12 ->
                    "Mäßigkeit"

                13 ->
                    "Humor"

                14 ->
                    "Hoffnung"

                15 ->
                    "Mut"

                16 ->
                    "Fleiß"

                _ ->
                    "Tugend"

        Back ->
            ""


value : Card -> Int
value card =
    case card of
        Joker ->
            0

        Binary n ->
            n

        Trump v ->
            v
                |> modBy 7
                |> (\n ->
                        if n == 0 then
                            7

                        else
                            n
                   )

        Element v ->
            v |> modBy 4

        Planet v ->
            v

        Virtue n ->
            n

        Back ->
            -1
