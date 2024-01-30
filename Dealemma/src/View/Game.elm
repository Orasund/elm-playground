module View.Game exposing (..)

import Card exposing (Card)
import Dict
import Game exposing (Game)
import Game.Entity
import Goal
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
import Layout
import View.Card
import View.Hand
import View.Ui


toHtml :
    { onChallenge : msg
    , onPlay : Card -> msg
    , yourTurn : Bool
    }
    -> Game
    -> Html msg
toHtml args game =
    [ game.opponentCards
        |> View.Hand.opponent [ Style.justifyContentCenter ]
    , [ "Last Call"
            |> Layout.text
                [ Html.Attributes.style "font-size" "24px"
                , Style.justifyContentCenter
                ]
      , [ Layout.el [ Layout.fill ] Layout.none
        , game.playedCards
            |> List.reverse
            |> List.map
                (\card ->
                    \attrs ->
                        View.Card.toHtml attrs
                            { probability =
                                game.probabilities
                                    |> Dict.get (Goal.description card.goal)
                                    |> Maybe.withDefault 0
                            }
                            card
                )
            |> List.map Game.Entity.new
            |> List.indexedMap
                (\i ->
                    Game.Entity.move
                        ( (List.length game.playedCards - i - 1) * 20 |> toFloat
                        , 0
                        )
                )
            |> Game.Entity.pileAbove (View.Card.empty [])
            |> Game.Entity.toHtml [ Style.justifyContentCenter ]
        , Layout.el [ Layout.fill ] Layout.none
        ]
            |> Layout.row
                [ Layout.contentWithSpaceBetween
                , Style.gap "8px"
                ]
      ]
        |> Layout.column
            [ Style.justifyContentCenter
            , Style.gap "8px"
            ]
    , [ (if not args.yourTurn then
            "Waiting..."

         else
            "Play a card with a smaller value or call the bluff"
        )
            |> Layout.text [ Style.justifyContentCenter ]
      , game.yourCards
            |> View.Hand.toHtml
                [ Style.justifyContentCenter
                ]
                { onPlay = args.onPlay
                , currentPercentage = Game.currentPercentage game
                , probabilities = game.probabilities
                }
      , View.Ui.button []
            { onPress =
                if args.yourTurn then
                    Just args.onChallenge

                else
                    Nothing
            , label =
                "Call the bluff for "
                    ++ (game.playedCards
                            |> List.head
                            |> Maybe.andThen
                                (\card ->
                                    game.probabilities
                                        |> Dict.get (Goal.description card.goal)
                                )
                            |> Maybe.withDefault 0
                            |> String.fromInt
                       )
                    ++ " CREDITS"
            }
            |> Layout.el [ Style.justifyContentCenter ]
      ]
        |> Layout.column [ Style.gap "16px" ]
    ]
        |> Layout.column
            [ Style.height "100%"
            , Layout.contentWithSpaceBetween
            ]
