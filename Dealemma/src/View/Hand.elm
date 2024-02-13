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
        , currentValue : Int
        , values : Dict String Int
        }
    -> List ( CardId, Card )
    -> Html msg
toHtml attrs args list =
    list
        |> List.sortBy
            (\( _, card ) ->
                args.values
                    |> Dict.get (Goal.description card.goal)
                    |> Maybe.withDefault 0
            )
        |> List.map
            (\( cardId, card ) ->
                let
                    value =
                        args.values
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                in
                ( cardId, card )
                    |> View.Card.toHtml
                        (if value >= args.currentValue then
                            Layout.asButton
                                { label = "play"
                                , onPress =
                                    Just (args.onPlay cardId)
                                }

                         else
                            []
                        )
                        { value = value
                        , faceUp = True
                        , active = value >= args.currentValue
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


opponent : List (Attribute msg) -> { values : Dict String Int } -> List ( CardId, Card ) -> Html msg
opponent attrs args list =
    list
        |> List.map
            (\( cardId, card ) ->
                let
                    value =
                        args.values
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                in
                View.Card.toHtml []
                    { value = value
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
