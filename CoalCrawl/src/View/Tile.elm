module View.Tile exposing (..)

import Config
import Data.Tile exposing (Tile(..))
import Data.Zoom exposing (Zoom)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout


toHtml : Zoom -> Tile -> Html msg
toHtml zoom tile =
    case tile of
        CharTile { content, color, size, bold, animation } ->
            content
                |> String.fromChar
                |> Html.text
                |> Layout.el
                    ([ Attr.style "width" (Config.tileSize zoom)
                     , Attr.style "height" (Config.tileSize zoom)
                     , Attr.style "font-size" (Config.fontSize size zoom)
                     , Attr.style "color" color
                     ]
                        ++ (if bold then
                                [ Attr.style "font-weight" "bold" ]

                            else
                                []
                           )
                        ++ (if animation then
                                [ Attr.class "animate__animated animate__pulse animate__infinite animate__faster" ]

                            else
                                []
                           )
                        ++ Layout.centered
                    )

        ImageTile { source, animation } ->
            Html.img
                ([ Attr.src source
                 , Attr.style "width" (Config.tileSize zoom)
                 , Attr.style "height" (Config.tileSize zoom)
                 ]
                    ++ (if animation then
                            [ Attr.class "animate__animated animate__pulse animate__infinite animate__faster" ]

                        else
                            []
                       )
                )
                []
