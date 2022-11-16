module View.Tile exposing (..)

import Config
import Data.Tile exposing (Tile)
import Data.Zoom exposing (Zoom)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout


toHtml : Zoom -> Tile -> Html msg
toHtml zoom tile =
    tile
        |> (\{ content, color, bold, animation } ->
                content
                    |> String.fromChar
                    |> Html.text
                    |> Layout.el
                        ([ Attr.style "width" (Config.tileSize zoom)
                         , Attr.style "height" (Config.tileSize zoom)
                         , Attr.style "font-size" (Config.tileSize zoom)
                         , Attr.style "color" color
                         ]
                            ++ (if bold then
                                    [ Attr.style "font-weight" "bold" ]

                                else
                                    []
                               )
                            ++ (if animation then
                                    [ Attr.class "animate__animated animate__pulse animate__infinite" ]

                                else
                                    []
                               )
                            ++ Layout.centered
                        )
           )
