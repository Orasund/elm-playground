module View.Card exposing (..)

import Game exposing (Card)
import Game.Card
import Goal
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style as Style
import Suit


height =
    "120px"


back : Html msg
back =
    Game.Card.default
        [ Style.height height
        , Html.Attributes.style "background-color" "#c6b8b8"
        ]
        []


small : Card -> Html msg
small card =
    Suit.icon card.suit
        |> Html.text
        |> Game.Card.back
            [ Style.height "60px"
            , Html.Attributes.style "font-size" "18px"
            , Html.Attributes.style "background-color" (Suit.color card.suit)
            ]


toHtml : List (Attribute msg) -> Card -> Html msg
toHtml attrs card =
    [ [ String.fromInt (Goal.probability card.goal)
            |> Html.text
            |> Game.Card.element
                []
      , Suit.icon card.suit
            |> Html.text
            |> Game.Card.element []
      ]
        |> Game.Card.row [ Html.Attributes.style "font-size" "18px" ]
    , card.goal
        |> Goal.goalDescription
        |> Html.text
        |> Game.Card.element [ Html.Attributes.style "text-align" "center" ]
    ]
        |> Game.Card.default
            ([ Style.height height
             , Html.Attributes.style "background-color" (Suit.color card.suit)
             ]
                ++ attrs
            )
