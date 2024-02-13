module View.Game exposing (..)

import Dict
import Game exposing (CardId, Game)
import Game.Area
import Game.Entity
import Goal
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Set
import View.Card
import View.Hand
import View.Ui


toHtml :
    { onChallenge : msg
    , onPlay : CardId -> msg
    , yourTurn : Bool
    }
    -> Game
    -> Html msg
toHtml args game =
    [ game.opponentCards
        |> Set.toList
        |> List.filterMap
            (\cardId ->
                cardId
                    |> Game.getCardFrom game
                    |> Maybe.map (Tuple.pair cardId)
            )
        |> View.Hand.opponent [ Style.justifyContentCenter ]
            { values = game.values }
    , [ "Last Call"
            |> Layout.text
                [ Html.Attributes.style "font-size" "24px"
                , Style.justifyContentCenter
                ]
      , [ Layout.el [ Layout.fill ] Layout.none
        , game.playedCards
            |> List.reverse
            |> List.filterMap
                (\cardId ->
                    cardId
                        |> Game.getCardFrom game
                        |> Maybe.map (Tuple.pair cardId)
                )
            |> List.map
                (\( cardId, card ) ->
                    View.Card.toHtml []
                        { value =
                            game.values
                                |> Dict.get (Goal.description card.goal)
                                |> Maybe.withDefault 0
                        , faceUp = True
                        , active = False
                        }
                        ( cardId, card )
                )
            |> List.indexedMap
                (\i ->
                    Game.Entity.move
                        ( (List.length game.playedCards - i - 1) * 20 |> toFloat
                        , 0
                        )
                )
            |> Game.Area.pileAbove ( 0, 0 ) ( "discardPile", View.Card.empty )
            |> Game.Area.toHtml [ Style.justifyContentCenter ]
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
            "Play a card with a higher value or call the bluff"
        )
            |> Layout.text [ Style.justifyContentCenter ]
      , game.yourCards
            |> Set.toList
            |> List.filterMap
                (\cardId ->
                    cardId
                        |> Game.getCardFrom game
                        |> Maybe.map (Tuple.pair cardId)
                )
            |> View.Hand.toHtml
                [ Style.justifyContentCenter
                ]
                { onPlay = args.onPlay
                , currentValue = Game.currentValue game
                , values = game.values
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
                                (\cardId ->
                                    cardId
                                        |> Game.getCardFrom game
                                        |> Maybe.map (Tuple.pair cardId)
                                )
                            |> Maybe.andThen
                                (\( _, card ) ->
                                    game.values
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
