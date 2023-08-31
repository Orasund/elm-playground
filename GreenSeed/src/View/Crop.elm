module View.Crop exposing (..)

import Crop exposing (Crop)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


asCircle : List (Attribute msg) -> Crop -> Html msg
asCircle attrs crop =
    crop
        |> Crop.toEmoji
        |> Layout.text ([ Html.Attributes.class "button icon-button" ] ++ attrs)
