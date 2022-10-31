module View.Tile exposing (..)

import Data.Tile exposing (Tile)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout


toHtml : Tile -> Html msg
toHtml tile =
    tile
        |> (\{ content, color, bold } ->
                content
                    |> String.fromChar
                    |> Html.text
                    |> Layout.el
                        ([ Attr.style "width" "32px"
                         , Attr.style "height" "32px"
                         , Attr.style "font-size" "24px"
                         , Attr.style "color" color
                         ]
                            ++ (if bold then
                                    [ Attr.style "font-weight" "bold" ]

                                else
                                    []
                               )
                            ++ Layout.centered
                        )
           )
