module View.Tile exposing (..)

import Config
import Data.Tile exposing (Tile)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout


toHtml : Tile -> Html msg
toHtml tile =
    tile
        |> (\{ content, color, bold, big } ->
                content
                    |> String.fromChar
                    |> Html.text
                    |> Layout.el
                        ([ Attr.style "width" Config.tileSize
                         , Attr.style "height" Config.tileSize
                         , Attr.style "font-size" Config.tileSize
                         , Attr.style "color" color
                         ]
                            ++ (if bold then
                                    [ Attr.style "font-weight" "bold"
                                    , Attr.class "animate__animated animate__pulse animate__infinite"
                                    ]

                                else
                                    []
                               )
                            ++ (if big then
                                    [ Attr.style "font-size" Config.tileSize ]

                                else
                                    []
                               )
                            ++ Layout.centered
                        )
           )
