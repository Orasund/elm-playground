module View.Card exposing (..)

import Card exposing (Card)
import Game exposing (CardId)
import Game.Card
import Game.Entity exposing (Entity)
import Html exposing (Attribute, Html)
import Html.Style
import Suit
import View.Goal


height : number
height =
    120


back : List (Attribute msg) -> Html msg
back attrs =
    Game.Card.default
        ([ Html.Style.heightPx height
         , Html.Style.backgroundColor "#c6b8b8"
         , Html.Style.boxSizingBorderBox
         ]
            ++ attrs
        )
        []


empty : List (Attribute msg) -> Html msg
empty attrs =
    Game.Card.empty ([ Html.Style.heightPx height, Html.Style.boxSizingBorderBox ] ++ attrs)
        "No card"


front : List (Attribute msg) -> { a | value : Int } -> Card -> Html msg
front attrs args card =
    [ [ String.fromInt args.value
            |> Html.text
            |> Game.Card.element []
      , Suit.icon card.suit
            |> Html.text
            |> Game.Card.element []
      ]
        |> Game.Card.row [ Html.Style.fontSizePx 18 ]
    , [ card.goal
            |> View.Goal.toHtml [ Html.Style.justifyContentCenter ]
                { big = False }
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.justifyContentCenter
            ]
        |> Game.Card.element [ Html.Style.justifyContentCenter ]
    ]
        |> Game.Card.default
            ([ Html.Style.heightPx height
             , Html.Style.backgroundColor (Suit.color card.suit)
             , Html.Style.boxSizingBorderBox
             ]
                ++ attrs
            )


toHtml : List (Attribute msg) -> { value : Int, faceUp : Bool, active : Bool } -> ( CardId, Card ) -> Entity ( String, List (Attribute msg) -> Html msg )
toHtml attrs args ( cardId, card ) =
    Game.Entity.flippable
        ([ Html.Style.heightPx height
         , Html.Style.widthPx (toFloat height * 2 / 3)
         ]
            ++ attrs
        )
        { front =
            (\a ->
                front
                    (if args.active then
                        Html.Style.border "4px solid #679aff" :: a

                     else
                        Html.Style.color "rgba(0,0,0,0.5)" :: a
                    )
                    args
                    card
            )
                |> Game.Entity.new
        , back = back |> Game.Entity.new
        , faceUp = args.faceUp
        }
        |> Game.Entity.map (Tuple.pair ("card_" ++ String.fromInt cardId))
