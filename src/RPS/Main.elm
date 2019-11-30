module RPS exposing (main)

import Element
import Emojidojo exposing (Game)


main : Game () Never
main =
    Emojidojo.define
        { title = "Rock Paper Scissors"
        , init = ()
        , view =
            \() ->
                Element.text "test"
        , config =
            Emojidojo.config
                { jsonstoreId = "41bda26d78d87269a31efaa657f4b9e12f39748413eb64513e4b1c50f95ed1bd"
                , version = 0.1001
                }
        }
