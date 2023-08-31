module View.MenuBar exposing (..)

import Html exposing (Html)
import Layout


toHtml : { openShop : msg, sleep : msg } -> Html msg
toHtml args =
    [ Layout.textButton []
        { label = "Shop"
        , onPress = Just args.openShop
        }
    , Layout.textButton []
        { label = "Sleep"
        , onPress = Just args.sleep
        }
    ]
        |> Layout.row []
