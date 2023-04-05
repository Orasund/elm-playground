module View exposing (..)

import Color
import Fish exposing (BitColor(..))
import Html exposing (Html)
import Html.Attributes
import Image
import Image.Color
import Random exposing (Seed)
import Rule


tank : { animationFrame : Bool } -> List (List ( Int, Int )) -> Html msg
tank args patterns =
    patterns
        |> List.reverse
        |> List.map (fish { animationFrame = args.animationFrame })
        |> Html.div []


fish : { animationFrame : Bool } -> List ( Int, Int ) -> Html msg
fish args pattern =
    Fish.withPattern { animate = args.animationFrame } pattern
        |> List.map
            (List.map
                (\bitColor ->
                    case bitColor of
                        Black ->
                            Color.black

                        Primary ->
                            Color.blue

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
