module View.Hand exposing (..)

import Card exposing (Card)
import Dict exposing (Dict)
import Game exposing (CardId)
import Goal
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style as Style
import Layout
import View.Card


toHtml :
    List (Attribute msg)
    ->
        { onPlay : CardId -> msg
        , currentPercentage : Int
        , probabilities : Dict String Int
        }
    -> List ( CardId, Card )
    -> Html msg
toHtml attrs args list =
    list
        |> List.sortBy
            (\( _, card ) ->
                args.probabilities
                    |> Dict.get (Goal.description card.goal)
                    |> Maybe.withDefault 0
            )
        |> List.map
            (\( cardId, card ) ->
                let
                    probability =
                        args.probabilities
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                in
                card
                    |> View.Card.toHtml
                        (if probability <= args.currentPercentage then
                            Html.Attributes.style "border" "4px solid #679aff"
                                :: Layout.asButton
                                    { label = "play"
                                    , onPress =
                                        Just (args.onPlay cardId)
                                    }

                         else
                            [ Html.Attributes.style "color" "rgba(0,0,0,0.5)"
                            ]
                        )
                        { probability = probability }
            )
        |> Layout.row (Style.gap "8px" :: attrs)


opponent : List (Attribute msg) -> List ( CardId, Card ) -> Html msg
opponent attrs list =
    list
        |> List.map (\_ -> View.Card.back)
        |> Layout.row (Style.gap "8px" :: attrs)
