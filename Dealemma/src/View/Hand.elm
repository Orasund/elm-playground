module View.Hand exposing (..)

import Card exposing (Card)
import Dict exposing (Dict)
import Goal
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Style as Style
import Layout
import View.Card


toHtml :
    List (Attribute msg)
    ->
        { onPlay : Card -> msg
        , currentPercentage : Int
        , probabilities : Dict String Int
        }
    -> List Card
    -> Html msg
toHtml attrs args list =
    list
        |> List.map
            (\card ->
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
                                        Just (args.onPlay card)
                                    }

                         else
                            [ Html.Attributes.style "color" "rgba(0,0,0,0.5)"
                            ]
                        )
                        { probability = probability }
            )
        |> Layout.row (Style.gap "8px" :: attrs)


opponent : List (Attribute msg) -> List Card -> Html msg
opponent attrs list =
    list
        |> List.map (\_ -> View.Card.back)
        |> Layout.row (Style.gap "8px" :: attrs)
