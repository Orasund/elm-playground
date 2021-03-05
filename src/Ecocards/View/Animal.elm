module Ecocards.View.Animal exposing (asCard, asCircle)

import Color exposing (Color)
import Ecocards.Data.Animal as Animal exposing (Animal)
import Ecocards.View.Biome as Biome
import Ecocards.View.Material as Material
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attributes
import Set
import Widget.Material.Color as MaterialColor


asCircle :
    Animal
    -> Element msg
asCircle animal =
    let
        title =
            animal.symbol

        color =
            animal.biome |> Biome.asColor
    in
    title
        |> Element.text
        |> Element.el
            [ Element.centerX, Element.centerY ]
        |> Element.el
            [ Element.height <| Element.px 36
            , Element.width <| Element.px 36
            , Border.rounded 18
            , color
                |> Material.scaleOpacity 0.36
                |> Material.fromColor
                |> Background.color
            ]


asCard :
    { onPress : Maybe msg
    , isSelected : Bool
    , isActive : Bool
    }
    -> Animal
    -> Element msg
asCard { onPress, isSelected, isActive } animal =
    let
        title =
            animal.symbol

        text =
            String.fromInt animal.strength

        color =
            animal.biome |> Biome.asColor
    in
    [ Input.button
        (Material.buttonFont
            {--++ (if onPress == Nothing then
                    [ Color.rgb255 127 127 127
                        |> Material.scaleOpacity MaterialColor.buttonDisabledOpacity
                        |> Material.fromColor
                        |> Border.color
                    , 
                    ]

                else
                    []
               )--}
            ++ [ (if isActive then
                    Color.rgb255 204 102 119

                  else
                    color
                 )
                    |> Material.scaleOpacity
                        (if isSelected then
                            MaterialColor.buttonSelectedOpacity

                         else if isActive then
                            1

                         else if onPress == Nothing then
                            0.36

                         else
                            1
                        )
                    |> Material.fromColor
                    |> Background.color
               , Element.width <| Element.px <| 70
               ]
            ++ (if onPress == Nothing then
                    [ Element.htmlAttribute <| Attributes.style "cursor" "not-allowed"
                    ]

                else if isActive then
                    [ Color.rgb255 255 255 255 |> Material.fromColor |> Font.color ]

                else
                    []
               )
            ++ [ Border.rounded 8
               , Element.padding 4
               ]
        )
        { onPress = onPress
        , label =
            [ title
                |> Element.text
                |> Element.el
                    [ Element.centerX
                    , Font.size 30
                    ]
            , text
                |> Element.text
                |> Element.el
                    [ Element.centerX
                    , Font.size 20
                    ]
            , animal.eats
                |> Set.toList
                |> List.filterMap Animal.biomeFromString
                |> List.map
                    (\biome ->
                        Element.el
                            [ biome
                                |> Biome.asColor
                                |> Material.fromColor
                                |> Background.color
                            , Element.width <| Element.px 10
                            , Element.height <| Element.px 10
                            , Border.rounded 5
                            , Border.width 1
                            , Color.rgb255 0 0 0
                                |> Material.scaleOpacity 0.5
                                |> Material.fromColor
                                |> Border.color
                            ]
                            Element.none
                    )
                |> Element.row
                    [ Element.spacing 5
                    , Element.centerX
                    ]
            ]
                |> Element.column
                    [ Element.centerY
                    , Element.centerX
                    , Element.spacing 10
                    ]
                |> Element.el
                    [ Element.width <| Element.px 66
                    , Element.height <| Element.px 80
                    ]
        }
    ]
        |> Element.column
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            , Element.spaceEvenly
            ]
