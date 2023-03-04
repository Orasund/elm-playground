module View exposing (..)

import Action
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


gray : String
gray =
    "#C0B8AF"


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

                Fear ->
                    gray
    in
    Game.Entity.flippable []
        { front =
            (\attr ->
                [ Card.emoji c
                    ++ " "
                    ++ Card.name c
                    |> Html.text
                    |> Game.Card.element []
                , Card.emoji c
                    |> Html.text
                    |> Layout.el
                        [ Html.Attributes.style "background-color" "white"
                        , Html.Attributes.style "aspect-ratio" "1"
                        , Html.Attributes.style "padding" "8px"
                        , Html.Attributes.style "border-radius" "100%"
                        ]
                    |> Game.Card.element
                        ([ Html.Attributes.style "font-size" "36px"
                         ]
                            ++ Layout.centered
                        )
                , Action.description c
                    |> Html.text
                    |> Game.Card.element
                        [ Layout.fill
                        , Html.Attributes.style "font-size" "0.9em"
                        ]
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
        Layout.none
            |> Game.Card.back
                ([ Html.Attributes.style "background" "linear-gradient(45deg, #dca 12%, transparent 0, transparent 88%, #dca 0),\n    linear-gradient(135deg, transparent 37%, #a85 0, #a85 63%, transparent 0),\n    linear-gradient(45deg, transparent 37%, #dca 0, #dca 63%, transparent 0) #753"
                 , Html.Attributes.style "background-size" "50px 50px"

                 -- Html.Attributes.style "background-color" "#3F784C"
                 , Html.Attributes.style "font-size" "38px"
                 , Html.Attributes.style "color" "white"
                 , Html.Attributes.style "text-align" "center"
                 ]
                    ++ attrs
                )
    )
        |> Game.Entity.new
