module View.Item exposing (..)

import Data.Item exposing (Item)
import Html exposing (Html)
import Html.Attributes


toHtml : Item -> Html msg
toHtml item =
    item
        |> Data.Item.toString
        |> String.toLower
        |> (\string -> "assets/svg/icon/" ++ string ++ ".svg")
        |> (\url -> Html.img [ Html.Attributes.src url, Html.Attributes.height 16 ] [])
