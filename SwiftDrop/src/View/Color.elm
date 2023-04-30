module View.Color exposing (..)


fromInt : Int -> String
fromInt int =
    case int of
        1 ->
            --Blue
            "#887CAF"

        2 ->
            "#8080B3"

        3 ->
            "#7887AB"

        4 ->
            "#718EA4"

        5 ->
            "#669999"

        6 ->
            "#75AF96"

        7 ->
            --green
            "#88CC88"

        8 ->
            "#B9E397"

        9 ->
            "#D4EE9F"

        10 ->
            "#E8F6A4"

        11 ->
            "#FFFFAA"

        12 ->
            "#FFF7AA"

        13 ->
            --yellow
            "#FFF0AA"

        14 ->
            "#FFEAAA"

        15 ->
            "#FFE3AA"

        16 ->
            "#FFDBAA"

        17 ->
            "#FFD1AA"

        18 ->
            "#FFC2AA"

        19 ->
            --red
            "#FFAAAA"

        20 ->
            "#E699AF"

        _ ->
            "#fff"
