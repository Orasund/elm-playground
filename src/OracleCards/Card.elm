module OracleCards.Card exposing (Card(..), color, description, title, value)

import OracleCards.View as View


type Card
    = Black Int
    | White Int
    | Trump Int
    | Emotion Int
    | Animal Int
    | Season Int
    | Direction Int


color : Card -> String
color card =
    case card of
        Black _ ->
            "white"

        White _ ->
            "black"

        Trump n ->
            if n <= 7 then
                View.blue

            else if n <= 14 then
                View.red

            else
                View.green

        _ ->
            "transparent"


title : Card -> String
title card =
    case card of
        Black _ ->
            "Yin"

        White _ ->
            "Yang"

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

        Animal n ->
            case n of
                1 ->
                    "Einhorn"

                2 ->
                    "Drache"

                3 ->
                    "Phönix"

                4 ->
                    "Schildkröte"

                _ ->
                    "Tier"

        Season n ->
            case n of
                1 ->
                    "Frühling"

                2 ->
                    "Sommer"

                3 ->
                    "Herbst"

                4 ->
                    "Winter"

                _ ->
                    "Jahreszeit"

        Direction n ->
            case n of
                1 ->
                    "Nordwind"

                2 ->
                    "Ostwind"

                3 ->
                    "Sommerwind"

                4 ->
                    "Herbstwind"

                _ ->
                    "Jahreszeit"


description : Card -> String
description card =
    case card of
        Black _ ->
            "Böse"

        White _ ->
            "Gut"

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

        Animal n ->
            case n of
                1 ->
                    "Einhorn"

                2 ->
                    "Drache"

                3 ->
                    "Phönix"

                4 ->
                    "Schildkröte"

                _ ->
                    "Tier"

        Season n ->
            case n of
                1 ->
                    "Optimistisch"

                2 ->
                    "Emotional"

                3 ->
                    "Kalkuliert"

                4 ->
                    "Pesimistisch"

                _ ->
                    "Jahreszeit"

        Direction n ->
            case n of
                1 ->
                    "Beruf"

                2 ->
                    "Gesundheit"

                3 ->
                    "Liebe"

                4 ->
                    "Geld"

                _ ->
                    "Jahreszeit"


value : Card -> Int
value card =
    case card of
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

        Animal v ->
            v

        Season v ->
            v

        Direction v ->
            v
