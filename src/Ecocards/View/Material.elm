module Ecocards.View.Material exposing (Palette, accessibleTextColor, accessibleWithTextColor, buttonDisabledOpacity, buttonFocusOpacity, buttonFont, buttonHoverOpacity, buttonPressedOpacity, buttonSelectedOpacity, dark, darkPalette, defaultPalette, fromCIELCH, fromColor, gray, h6, scaleOpacity, shadow, textAndBackground, toCIELCH, withShade)

import Color exposing (Color)
import Color.Accessibility as Accessibility
import Color.Convert as Convert
import Element exposing (Attribute)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes as Attributes



{-------------------------------------------------------------------------------
-- T Y P O G R A P H Y
-------------------------------------------------------------------------------}


buttonFont : List (Attribute msg)
buttonFont =
    [ Element.htmlAttribute <| Attributes.style "text-transform" "uppercase"
    , Font.size 14
    , Font.semiBold --medium
    , Font.letterSpacing 1.25
    ]


h6 : List (Attribute msg)
h6 =
    [ Font.size 20
    , Font.semiBold --medium
    , Font.letterSpacing 0.15
    ]



{-------------------------------------------------------------------------------
-- C O L O R
-------------------------------------------------------------------------------}


{-| The material design comes with customizable color palettes.
Check out [the official documentation about the color system](https://material.io/design/color/the-color-system.html#color-theme-creation) to see how these colors are used.
For the `-on` colors you can use white, for transitions into white, or black,for transitions into black. Other colors are also possible, but i've not seen any website acutally using a different color.
-}
type alias Palette =
    { primary : Color --Color.rgb255 0x62 0x00 0xEE
    , secondary : Color --Color.rgb255 0x03 0xda 0xc6
    , background : Color --Color.rgb255 0xFF 0xFF 0xFF
    , surface : Color --Color.rgb255 0xFF 0xFF 0xFF
    , error : Color --Color.rgb255 0xB0 0x00 0x20
    , on :
        { primary : Color --Color.rgb255 0xFF 0xFF 0xFF
        , secondary : Color --Color.rgb255 0x00 0x00 0x00
        , background : Color --Color.rgb255 0x00 0x00 0x00
        , surface : Color --Color.rgb255 0x00 0x00 0x00
        , error : Color --Color.rgb255 0xFF 0xFF 0xFF
        }
    }


{-| The default color theme.
![The default theme](https://lh3.googleusercontent.com/k6WO1fd7T40A9JvSVfHqs0CPLFyTEDCecsVGxEDhOaTP0wUTPYOVVkxt60hKxBprgNoMqs8OyKqtlaQ4tDBtQJs-fTcZrpZEjxhUVQ=w1064-v0)
_Image take from [material.io](https://material.io/design/color/the-color-system.html#color-theme-creation)_
-}
defaultPalette : Palette
defaultPalette =
    { primary = Color.rgb255 0x62 0x00 0xEE
    , secondary = Color.rgb255 0x03 0xDA 0xC6
    , background = Color.rgb255 0xFF 0xFF 0xFF
    , surface = Color.rgb255 0xFF 0xFF 0xFF
    , error = Color.rgb255 0xB0 0x00 0x20
    , on =
        { primary = Color.rgb255 0xFF 0xFF 0xFF
        , secondary = Color.rgb255 0x00 0x00 0x00
        , background = Color.rgb255 0x00 0x00 0x00
        , surface = Color.rgb255 0x00 0x00 0x00
        , error = Color.rgb255 0xFF 0xFF 0xFF
        }
    }


{-| The offical dark theme of google.
![The dark theme](https://lh3.googleusercontent.com/tv7J2o4ZiSmLYwyBslBs_PLzKyzI8QUV5qdvHGfoAQn9r7pY4Hj5SmW27m3zUWeDtRSE8Cb5_5PQmkbavDfw7XbIL8EodIKZhilRdg=w1064-v0)
_Image take from [material.io](https://material.io/design/color/dark-theme.html#ui-application)_
-}
darkPalette : Palette
darkPalette =
    { primary = Color.rgb255 0xBB 0x86 0xFC
    , secondary = Color.rgb255 0x03 0xDA 0xC6
    , background = Color.rgb255 0x12 0x12 0x12
    , surface = Color.rgb255 0x12 0x12 0x12
    , error = Color.rgb255 0xCF 0x66 0x79
    , on =
        { primary = Color.rgb255 0x00 0x00 0x00
        , secondary = Color.rgb255 0x00 0x00 0x00
        , background = Color.rgb255 0xFF 0xFF 0xFF
        , surface = Color.rgb255 0xFF 0xFF 0xFF
        , error = Color.rgb255 0x00 0x00 0x00
        }
    }


buttonHoverOpacity : Float
buttonHoverOpacity =
    0.08


buttonFocusOpacity : Float
buttonFocusOpacity =
    0.24


buttonPressedOpacity : Float
buttonPressedOpacity =
    0.32


buttonDisabledOpacity : Float
buttonDisabledOpacity =
    0.38


buttonSelectedOpacity : Float
buttonSelectedOpacity =
    0.16


accessibleTextColor : Color -> Color
accessibleTextColor color =
    let
        l : Float
        l =
            1
                + (color |> Color.toRgba |> .alpha)
                * (Accessibility.luminance color - 1)

        ratioBlack : Float
        ratioBlack =
            1.05 / (l + 0.05)

        ratioWhite : Float
        ratioWhite =
            (l + 0.05) / 0.05
    in
    if ratioBlack < ratioWhite then
        Color.rgb255 0 0 0

    else
        Color.rgb255 255 255 255


accessibleWithTextColor : Color -> Color -> Color
accessibleWithTextColor c color =
    let
        l1 : Float
        l1 =
            1
                + (c |> Color.toRgba |> .alpha)
                * (Accessibility.luminance c - 1)

        l2 : Float
        l2 =
            1
                + (color |> Color.toRgba |> .alpha)
                * (Accessibility.luminance color - 1)

        newConstrast : Float
        newConstrast =
            7

        lighterLuminance : Float
        lighterLuminance =
            newConstrast * (l2 + 0.05) - 0.05

        darkerLuminance : Float
        darkerLuminance =
            (l2 + 0.05) - 0.05 / newConstrast
    in
    c
        |> (if l1 > l2 then
                if ((l1 + 0.05) / (l2 + 0.05)) < 7 then
                    Convert.colorToLab
                        >> (\col ->
                                { col | l = 100 * lighterLuminance }
                           )
                        >> Convert.labToColor

                else
                    identity

            else if ((l2 + 0.05) / (l1 + 0.05)) < 7 then
                Convert.colorToLab
                    >> (\col ->
                            { col | l = 100 * darkerLuminance }
                       )
                    >> Convert.labToColor

            else
                identity
           )


toCIELCH : Color -> { l : Float, c : Float, h : Float }
toCIELCH =
    Convert.colorToLab
        >> (\{ l, a, b } ->
                { l = l
                , c = sqrt (a * a + b * b)
                , h = atan2 b a
                }
           )


fromCIELCH : { l : Float, c : Float, h : Float } -> Color
fromCIELCH =
    (\{ l, c, h } ->
        { l = l
        , a = c * cos h
        , b = c * sin h
        }
    )
        >> Convert.labToColor


{-| using noahzgordon/elm-color-extra for colors
-}
withShade : Color -> Float -> Color -> Color
withShade c2 amount c1 =
    let
        alpha =
            c1
                |> Color.toRgba
                |> .alpha

        fun a b =
            { l = (a.l * (1 - amount) + b.l * amount) / 1
            , c = (a.c * (1 - amount) + b.c * amount) / 1
            , h = (a.h * (1 - amount) + b.h * amount) / 1
            }
    in
    fun (toCIELCH c1) (toCIELCH c2)
        |> fromCIELCH
        |> Color.toRgba
        |> (\color -> { color | alpha = alpha })
        |> Color.fromRgba


scaleOpacity : Float -> Color -> Color
scaleOpacity opacity =
    Color.toRgba
        >> (\color -> { color | alpha = color.alpha * opacity })
        >> Color.fromRgba


gray : Color
gray =
    Color.rgb255 0x77 0x77 0x77


dark : Color
dark =
    Color.rgb255 50 50 50


fromColor : Color -> Element.Color
fromColor =
    Color.toRgba >> Element.fromRgb


shadow :
    Float
    ->
        { offset : ( Float, Float )
        , size : Float
        , blur : Float
        , color : Element.Color
        }
shadow float =
    { color = Element.rgba255 0x00 0x00 0x00 0.2
    , offset = ( 0, float )
    , size = 0
    , blur = float
    }


textAndBackground : Color -> List (Element.Attr decorative msg)
textAndBackground color =
    [ color
        |> fromColor
        |> Background.color
    , color
        |> accessibleTextColor
        |> fromColor
        |> Font.color
    ]
