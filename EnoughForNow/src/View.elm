module View exposing (..)

import Card exposing (Card(..))
import Game.Card
import Game.Entity exposing (Entity)
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


green : String
green =
    "#80ED99"


yellow : String
yellow =
    "#EEB868"


red : String
red =
    "#EF767A"


blue : String
blue =
    "#77A6B6"


card : List (Attribute msg) -> Bool -> ( Int, Card ) -> Entity ( String, List (Attribute msg) -> Html msg )
card attrs faceUp ( cardId, c ) =
    let
        color =
            case c of
                Food ->
                    red

                Wood ->
                    green

                Stone ->
                    blue
    in
    Game.Entity.flippable []
        { front =
            (\attr ->
                [ Card.emoji c
                    ++ " "
                    ++ Card.name c
                    |> Html.text
                    |> Game.Card.element []
                , [ Card.emoji c
                        |> Html.text
                  ]
                    |> Html.div [ Html.Attributes.style "font-size" "64px" ]
                    |> Game.Card.element
                        ([ Layout.fill
                         , Html.Attributes.style "background-color" "white"
                         ]
                            ++ Layout.centered
                        )
                ]
                    |> Game.Card.default
                        (Html.Attributes.style "background-color" color :: attrs ++ attr)
            )
                |> Game.Entity.new
        , back = cardBack
        , faceUp = faceUp
        }
        |> Game.Entity.map (Tuple.pair ("card_" ++ String.fromInt cardId))


cardBack : Entity (List (Attribute msg) -> Html msg)
cardBack =
    (\attrs ->
        Html.text "Enough For Now"
            |> Game.Card.back
                ([ Html.Attributes.style "background" "linear-gradient(45deg, #dca 12%, transparent 0, transparent 88%, #dca 0),\n    linear-gradient(135deg, transparent 37%, #a85 0, #a85 63%, transparent 0),\n    linear-gradient(45deg, transparent 37%, #dca 0, #dca 63%, transparent 0) #753"
                 , Html.Attributes.style "background-size" "25px 25px"
                 , Html.Attributes.style "size" "40px"
                 ]
                    ++ attrs
                )
    )
        |> Game.Entity.new
