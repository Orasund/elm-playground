module View exposing (..)

import Color
import Fish exposing (BitColor(..))
import Html exposing (Html)
import Html.Attributes
import Image
import Image.Color


fish : List ( Int, Int ) -> Html msg
fish pattern =
    Fish.withPattern pattern
        |> List.map
            (List.map
                (\bitColor ->
                    case bitColor of
                        Black ->
                            Color.black

                        Primary ->
                            Color.red

                        Secondary ->
                            Color.white

                        None ->
                            Color.fromRgba
                                { red = 0
                                , blue = 0
                                , green = 0
                                , alpha = 0
                                }
                )
            )
        |> Image.Color.fromList2d
        |> Image.toPngUrl
        |> (\url ->
                Html.img
                    [ Html.Attributes.src url
                    , Html.Attributes.style "width" "64px"
                    , Html.Attributes.style "image-rendering" "pixelated"
                    ]
                    []
           )
