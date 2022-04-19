module Zess.View.Figure exposing (view)

import Dict
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Zess.Data.Figure as Figure exposing (Figure)


view : Bool -> List (Attribute msg) -> String -> Html msg
view isWhite attrs string =
    "assert/"
        ++ String.toLower string
        ++ (if isWhite then
                "_white"

            else
                "_black"
           )
        ++ ".svg"
        |> (\url ->
                Html.img
                    ([ Attr.src url
                     , Attr.alt
                        ((if isWhite then
                            "White "

                          else
                            "Black "
                         )
                            ++ string
                        )
                     ]
                        ++ attrs
                    )
                    []
           )
