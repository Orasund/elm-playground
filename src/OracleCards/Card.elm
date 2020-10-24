module OracleCards.Card exposing (Card(..), color, description, title, value)

import OracleCards.View as View


type Card
    = Black Int
    | White Int
    | Trump Int
    | Emotion Int
    | Season Int
    | Planet Int
    | Joker


color : Card -> String
color card =
    case card of
        Black _ ->
            "none"

        White _ ->
            "none"

        Trump n ->
            if n <= 7 then
                View.blue

            else if n <= 14 then
                View.red

            else
                View.green

        Emotion _ ->
            "white"

        Planet _ ->
            "white"

        Season _ ->
            "white"

        _ ->
            "none"


title : Card -> String
title card =
    case card of
        Joker ->
            "Joker"

        Black _ ->
            "Dunkelheit"

        White _ ->
            "Licht"

        Trump n ->
            case n of
                1 ->
                    "I - Der Magier"

                2 ->
                    "II - Die Hohepriesterin"

                3 ->
                    "III - Die Herrscherin"

                4 ->
                    "IV - Der Herrscher"

                5 ->
                    "V - Der Hierophant"

                6 ->
                    "VI - Die Liebenden"

                7 ->
                    "VII - Der Wagen"

                8 ->
                    "VIII - Die Gerechtigkeit"

                9 ->
                    "IX - Der Eremit"

                10 ->
                    "X - Das Rad des Schicksals"

                11 ->
                    "XI - Die Kraft"

                12 ->
                    "XII - Der Gehängte"

                13 ->
                    "XIII - Der Tod"

                14 ->
                    "XIV - Die Mäßigkeit"

                15 ->
                    "XV - Der Teufel"

                16 ->
                    "XVI - Der Turm"

                17 ->
                    "XVII - Der Stern"

                18 ->
                    "XVIII - Der Mond"

                19 ->
                    "XIX - Die Sonne"

                20 ->
                    "XX - Das Gericht"

                21 ->
                    "XXI - Die Welt"

                _ ->
                    "Trumpf"

        Emotion n ->
            case n of
                1 ->
                    "Freude"

                2 ->
                    "Trauer"

                3 ->
                    "Angst"

                4 ->
                    "Wut"

                _ ->
                    "Emotion"

        Season n ->
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
                    "Erde"

                4 ->
                    "Mars"

                5 ->
                    "Jupiter"

                6 ->
                    "Saturn"

                7 ->
                    "Uranus"

                8 ->
                    "Neptum"

                _ ->
                    "Planet"


description : Card -> String
description card =
    case card of
        Joker ->
            "Sorglosigkeit"

        Black _ ->
            "Revolution"

        White _ ->
            "Sicherheit"

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

        Emotion n ->
            case n of
                1 ->
                    "Freude"

                2 ->
                    "Trauer"

                3 ->
                    "Angst"

                4 ->
                    "Wut"

                _ ->
                    "Emotion"

        Season n ->
            case n of
                1 ->
                    "Optimistisch"

                2 ->
                    "Emotional"

                3 ->
                    "Realistisch"

                4 ->
                    "Kalkuliert"

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
                    "Annerkennung"

                7 ->
                    "Eigentum"

                8 ->
                    "Freiheit"

                _ ->
                    "Jahreszeit"


value : Card -> Int
value card =
    case card of
        Joker ->
            0

        Black v ->
            v

        White v ->
            v

        Trump v ->
            v
                |> modBy 7
                |> (\n ->
                        if n == 0 then
                            7

                        else
                            n
                   )

        Emotion v ->
            v

        Season v ->
            v

        Planet v ->
            v
