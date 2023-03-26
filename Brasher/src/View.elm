module View exposing (..)

import Config
import Game.Card
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs =
    Game.Card.default
        ([ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px")
         , Html.Attributes.style "width" (String.fromFloat (Config.cardHeight * 2 / 3) ++ "px")
         , Layout.contentWithSpaceBetween
         , Layout.alignAtCenter
         , Html.Attributes.style "padding" "16px"
         , Html.Attributes.style "aspect-ratio" "auto"
         ]
            ++ attrs
        )


emptyCard : Html msg
emptyCard =
    Game.Card.empty
        [ Html.Attributes.style "height" (String.fromFloat Config.cardHeight ++ "px")
        , Html.Attributes.style "width" (String.fromFloat (Config.cardHeight * 2 / 3) ++ "px")
        , Html.Attributes.style "padding" "16px"
        , Html.Attributes.style "aspect-ratio" "auto"
        ]
        "Please Wait"
