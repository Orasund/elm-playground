module View.Card exposing (..)

import Card exposing (Card)
import Game exposing (CardId)
import Game.Card
import Game.Entity exposing (Entity)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Suit
import View.Goal


height : Int
height =
    120


back : List (Attribute msg) -> Html msg
back attrs =
    Game.Card.default
        ([ Style.height (String.fromInt height ++ "px")
         , Html.Attributes.style "background-color" "#c6b8b8"
         , Style.boxSizingBorderBox
         ]
            ++ attrs
        )
        []


empty : List (Attribute msg) -> Html msg
empty attrs =
    Game.Card.empty ([ Style.height (String.fromInt height ++ "px"), Style.boxSizingBorderBox ] ++ attrs)
        "No card"


front : List (Attribute msg) -> { a | probability : Int } -> Card -> Html msg
front attrs args card =
    [ [ String.fromInt args.probability
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
            ([ Style.height (String.fromInt height ++ "px")
             , Html.Attributes.style "background-color" (Suit.color card.suit)
             , Style.boxSizingBorderBox
             ]
                ++ attrs
            )


toHtml : List (Attribute msg) -> { probability : Int, faceUp : Bool, active : Bool } -> ( CardId, Card ) -> Entity ( String, List (Attribute msg) -> Html msg )
toHtml attrs args ( cardId, card ) =
    Game.Entity.flippable
        ([ Style.height (String.fromInt height ++ "px")
         , Style.width (String.fromFloat (toFloat height * 2 / 3) ++ "px")
         ]
            ++ attrs
        )
        { front =
            (\a ->
                front
                    (if args.active then
                        [ Html.Attributes.style "border" "4px solid #679aff" ] ++ a

                     else
                        [ Html.Attributes.style "color" "rgba(0,0,0,0.5)" ] ++ a
                    )
                    args
                    card
            )
                |> Game.Entity.new
        , back = back |> Game.Entity.new
        , faceUp = args.faceUp
        }
        |> Game.Entity.map (Tuple.pair ("card_" ++ String.fromInt cardId))
