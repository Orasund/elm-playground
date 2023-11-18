module Css exposing (square, empty, bottom_left, top_left, top_right, bottom_right)

import Html
import Html.Attributes


square : Html.Attribute msg
square =
    Html.Attributes.class "square"


empty : Html.Attribute msg
empty =
    Html.Attributes.class "empty"


bottom_left : Html.Attribute msg
bottom_left =
    Html.Attributes.class "bottom-left"


top_left : Html.Attribute msg
top_left =
    Html.Attributes.class "top-left"


top_right : Html.Attribute msg
top_right =
    Html.Attributes.class "top-right"


bottom_right : Html.Attribute msg
bottom_right =
    Html.Attributes.class "bottom-right"
