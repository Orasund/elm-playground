module View.Card exposing (..)

import Game exposing (Card)
import Game.Card
import Goal
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Suit
import View.Goal


height =
    "120px"


back : Html msg
back =
    Game.Card.default
        [ Style.height height
        , Html.Attributes.style "background-color" "#c6b8b8"
        , Style.boxSizingBorderBox
        ]
        []


big : List (Attribute msg) -> Card -> Html msg
big attrs =
    toHtml (Style.height "200px" :: attrs)


empty : List (Attribute msg) -> Html msg
empty attrs =
    Game.Card.empty ([ Style.height height, Style.boxSizingBorderBox ] ++ attrs)
        "No card"


toHtml : List (Attribute msg) -> Card -> Html msg
toHtml attrs card =
    [ [ String.fromInt (Goal.probability card.goal)
            |> Html.text
            |> Game.Card.element []
      , Suit.icon card.suit
            |> Html.text
            |> Game.Card.element []
      ]
        |> Game.Card.row [ Html.Attributes.style "font-size" "18px" ]
    , [ card.goal
            |> View.Goal.toHtml [ Style.justifyContentCenter ]
                { big = False }
      ]
        |> Layout.column [ Style.justifyContentCenter ]
        |> Game.Card.element [ Style.justifyContentCenter ]
    ]
        |> Game.Card.default
            ([ Style.height height
             , Html.Attributes.style "background-color" (Suit.color card.suit)
             , Style.boxSizingBorderBox
             ]
                ++ attrs
            )
