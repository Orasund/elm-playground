module Zess.View.Figure exposing (view)

import Dict
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Zess.Data.Figure as Figure exposing (Figure)


view : List (Attribute msg) -> String -> Html msg
view attrs string =
    "assert/"
        ++ string
        ++ ".svg"
        |> (\url -> Html.img (Attr.src url :: attrs) [])
