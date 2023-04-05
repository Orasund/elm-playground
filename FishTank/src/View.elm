module View exposing (..)

import Color exposing (Color)
import Fish exposing (BitColor(..), Fish)
import Html exposing (Html)
import Html.Attributes
import Image
import Image.Color
import Layout
import Rule exposing (Pattern(..))
import Svg.Path
import Svg.Writer


tank : { animationFrame : Bool } -> List Fish -> Html msg
tank args patterns =
    patterns
        |> List.reverse
        |> List.map (fish { animationFrame = args.animationFrame })
        |> Layout.row [ Layout.gap 16 ]


fish : { animationFrame : Bool } -> Fish -> Html msg
fish args f =
    Fish.toBitmap { animate = args.animationFrame } f.pattern
        |> List.map
            (List.map
                (\bitColor ->
                    case bitColor of
                        Black ->
                            Color.black

                        Primary ->
                            f.primary

                        Secondary ->
                            f.secondary

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
                Layout.column []
                    [ Html.img
                        [ Html.Attributes.src url
                        , Html.Attributes.style "width" "64px"
                        , Html.Attributes.style "image-rendering" "pixelated"
                        ]
                        []
                    , f.rules
                        |> List.map
                            (\( b, p ) ->
                                patternCircle
                                    (if b then
                                        f.secondary

                                     else
                                        f.primary
                                    )
                                    p
                            )
                        |> Layout.row []
                    ]
           )


patternCircle : Color -> Pattern -> Html msg
patternCircle color p =
    (case p of
        Horizontal ->
            Svg.Path.startAt ( 0, 10 )
                |> Svg.Path.drawLineTo ( 20, 10 )
                |> Svg.Path.end

        Vertical ->
            Svg.Path.startAt ( 10, 0 )
                |> Svg.Path.drawLineTo ( 10, 20 )
                |> Svg.Path.end

        BottomUp ->
            Svg.Path.startAt ( 0, 20 )
                |> Svg.Path.drawLineTo ( 20, 0 )
                |> Svg.Path.end

        TopDown ->
            Svg.Path.startAt ( 0, 0 )
                |> Svg.Path.drawLineTo ( 20, 20 )
                |> Svg.Path.end
    )
        |> Svg.Writer.path
        |> Svg.Writer.withStrokeWidth 5
        |> Svg.Writer.withStrokeColor (Color.toCssString color)
        |> List.singleton
        |> Svg.Writer.toHtml
            [ Html.Attributes.style "width" "20px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "border-radius" "100%"
            , Html.Attributes.style "border" ("5px solid " ++ Color.toCssString color)
            ]
            { width = 20, height = 20 }
