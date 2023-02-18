module Game.Card exposing (..)

{-| ratio of width to height
-}

import Html exposing (Attribute, Html)
import Html.Attributes


{-| Displays an default view of a card.
-}
card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs content =
    Html.div
        ([ --Flexbox
           Html.Attributes.style "display" "flex"
         , Html.Attributes.style "flex-direction" "column"

         -- Aspect-ratio
         , ratio 0.66
         , Html.Attributes.style "height" "200px"

         -- Rounded Edges
         , Html.Attributes.style "border-radius" "16px"
         , Html.Attributes.style "overflow" "hidden"

         -- Defaults
         , Html.Attributes.style "background-color" "white"
         , Html.Attributes.style "border" "1px solid rgba(0, 0, 0, 0.2)"
         , Html.Attributes.style "font-size" "0.8em"
         , Html.Attributes.style "z-index" "1"
         ]
            ++ attrs
        )
        content


empty : List (Attribute msg) -> String -> Html msg
empty attrs string =
    card
        ([ Html.Attributes.style "border-style" "dashed"
         , Html.Attributes.style "color" "rgba(0, 0, 0, 0.5)"
         , Html.Attributes.style "justify-content" "center"
         , Html.Attributes.style "align-items" "center"
         , Html.Attributes.style "background-color" "none"
         , Html.Attributes.style "z-index" "0"
         ]
            ++ attrs
        )
        [ Html.div [ Html.Attributes.style "display" "flex" ] [ Html.text string ] ]


title : List (Attribute msg) -> Html msg -> Html msg
title attrs content =
    Html.div
        ([ Html.Attributes.style "padding" "8px 8px"
         , Html.Attributes.style "display" "flex"
         ]
            ++ attrs
        )
        [ content ]


backgroundImage : String -> List (Attribute msg)
backgroundImage src =
    [ Html.Attributes.style "background-image" ("url(" ++ src ++ ")")
    , Html.Attributes.style "background-size" "cover"
    , Html.Attributes.style "background-position" "center"
    ]


fillingImage : List (Attribute msg) -> String -> Html msg
fillingImage attrs src =
    Html.div
        ([ Html.Attributes.style "flex-grow" "1"
         , Html.Attributes.style "display" "flex"
         ]
            ++ backgroundImage src
            ++ attrs
        )
        []


description : List (Attribute msg) -> Html msg -> Html msg
description attrs content =
    Html.div
        ([ Html.Attributes.style "display" "flex"
         , Html.Attributes.style "padding" "8px 8px"
         ]
            ++ attrs
        )
        [ content ]



{--Html.div
        ([ Html.Attributes.style "flex" "1"
         , Html.Attributes.style "display" "flex"
         ]
            ++ backgroundImage src
            ++ attrs
        )
        []--}


{-| A row on top of the card. It uses flexbox.
-}
header : List (Attribute msg) -> List (Html msg) -> Html msg
header attrs content =
    Html.div
        ([ Html.Attributes.style "display" "flex"
         , Html.Attributes.style "flex-direction" "row"
         , Html.Attributes.style "justify-content" "space-between"
         , Html.Attributes.style "padding" "8px 8px"
         ]
            ++ attrs
        )
        content


{-| Defines a aspect-ratio of an element.

    card =
        Html.div
            [ ratio (2 / 3)
            , Html.Attributes.style "height" "200px"
            , Html.Attributes.style "border-radius" "16px"
            , Html.Attributes.style "background-color" "white"
            ]

-}
ratio : Float -> Attribute msg
ratio float =
    float
        |> String.fromFloat
        |> Html.Attributes.style "aspect-ratio"


type alias Transformation =
    String


transform : List Transformation -> Attribute msg
transform list =
    (if list == [] then
        "unset"

     else
        list
            |> String.join " "
    )
        |> Html.Attributes.style "transform"


zoom : Float -> Transformation
zoom float =
    "scale("
        ++ String.fromFloat float
        ++ ","
        ++ String.fromFloat float
        ++ ")"


rotate : Float -> Transformation
rotate float =
    "rotate("
        ++ String.fromFloat float
        ++ "rad)"


move : ( Float, Float ) -> Transformation
move ( x, y ) =
    "translate("
        ++ String.fromFloat x
        ++ "px,"
        ++ String.fromFloat y
        ++ "px)"
