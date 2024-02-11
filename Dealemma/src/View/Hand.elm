module View.Hand exposing (..)

import Card exposing (Card)
import Dict exposing (Dict)
import Game exposing (CardId)
import Game.Area
import Goal
import Html exposing (Attribute, Html)
import Html.Style
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
                ( cardId, card )
                    |> View.Card.toHtml
                        (if probability <= args.currentPercentage then
                            Layout.asButton
                                { label = "play"
                                , onPress =
                                    Just (args.onPlay cardId)
                                }

                         else
                            []
                        )
                        { probability = probability
                        , faceUp = True
                        , active = probability <= args.currentPercentage
                        }
            )
        |> Game.Area.withPolarPosition
            { minAngle = 0
            , maxAngle = 0
            , minDistance = -150
            , maxDistance = 150
            }
        |> Game.Area.toHtml
            (Html.Style.height (String.fromInt View.Card.height ++ "px") :: attrs)


opponent : List (Attribute msg) -> { probabilities : Dict String Int } -> List ( CardId, Card ) -> Html msg
opponent attrs args list =
    list
        |> List.map
            (\( cardId, card ) ->
                let
                    probability =
                        args.probabilities
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                in
                View.Card.toHtml []
                    { probability = probability
                    , faceUp = False
                    , active = False
                    }
                    ( cardId
                    , card
                    )
            )
        |> Game.Area.withPolarPosition
            { minAngle = 0
            , maxAngle = 0
            , minDistance = -150
            , maxDistance = 150
            }
        |> Game.Area.toHtml attrs
