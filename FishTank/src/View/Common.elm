module View.Common exposing (..)

import Color exposing (Color)
import Config
import Dict
import Fish exposing (BitColor(..), Fish, FishId)
import Game exposing (Game, TankId)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Keyed
import Image
import Image.Color
import Layout
import Rule exposing (Pattern(..))
import Set
import Svg.Path
import Svg.Writer
import Tank


storage :
    { onClick : FishId -> msg
    }
    -> Game
    -> Html msg
storage args g =
    g.storage
        |> Set.toList
        |> List.filterMap
            (\fishId ->
                g.fish
                    |> Dict.get fishId
                    |> Maybe.map (Tuple.pair fishId)
            )
        |> List.map
            (\( fishId, fish ) ->
                fish
                    |> fishSprite
                        (Layout.asButton
                            { label = "Load"
                            , onPress = Just (args.onClick fishId)
                            }
                        )
                        { animationFrame = False }
            )
        |> Layout.row [ Layout.gap 16, Layout.alignAtEnd ]


money : Int -> Html msg
money int =
    String.fromInt int
        |> Layout.text
            [ Html.Attributes.style "font-size" "3rem"
            ]


fishSprite : List (Attribute msg) -> { animationFrame : Bool } -> Fish -> Html msg
fishSprite attrs args f =
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
                Html.img
                    ([ Html.Attributes.src url
                     , Html.Attributes.style "width" (String.fromInt (Config.fishMinSize + 2 * f.size) ++ "px")
                     , Html.Attributes.style "image-rendering" "pixelated"
                     ]
                        ++ attrs
                    )
                    []
           )


fishInfo : Fish -> Html msg
fishInfo fish =
    let
        ( rules1, rules2 ) =
            fish.rules
                |> List.map
                    (\( b, p ) ->
                        ( b
                        , patternCircle
                            (if b then
                                fish.secondary

                             else
                                fish.primary
                            )
                            p
                        )
                    )
                |> List.partition Tuple.first
    in
    [ rules1
        |> List.map Tuple.second
        |> Layout.row []
    , rules2
        |> List.map Tuple.second
        |> Layout.row []
    ]
        |> Layout.column []


patternCircle : Color -> Pattern -> Html msg
patternCircle color p =
    (case p of
        Horizontal ->
            Svg.Path.startAt ( 0, 5 )
                |> Svg.Path.drawLineTo ( 10, 5 )
                |> Svg.Path.end

        Vertical ->
            Svg.Path.startAt ( 5, 0 )
                |> Svg.Path.drawLineTo ( 5, 10 )
                |> Svg.Path.end

        BottomUp ->
            Svg.Path.startAt ( 0, 10 )
                |> Svg.Path.drawLineTo ( 10, 0 )
                |> Svg.Path.end

        TopDown ->
            Svg.Path.startAt ( 0, 0 )
                |> Svg.Path.drawLineTo ( 10, 10 )
                |> Svg.Path.end
    )
        |> Svg.Writer.path
        |> Svg.Writer.withStrokeWidth 3
        |> Svg.Writer.withStrokeColor (Color.toCssString color)
        |> List.singleton
        |> Svg.Writer.toHtml
            [ Html.Attributes.style "border-radius" "100%"
            , Html.Attributes.style "border" ("3px solid " ++ Color.toCssString color)
            ]
            { width = 10, height = 10 }
