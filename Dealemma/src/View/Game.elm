module View.Game exposing (..)

import Game exposing (Card, Game)
import Game.Entity
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
    , game.playedCards
        |> List.reverse
        |> List.map
            (\card ->
                \attrs -> View.Card.toHtml attrs card
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
    , [ (if not args.yourTurn then
            "Waiting..."

         else
            "Pick a card with a smaller value or challenge your opponent"
        )
            |> Layout.text []
      , game.yourCards
            |> View.Hand.toHtml
                [ Style.justifyContentCenter
                ]
                { onPlay = args.onPlay
                , currentPercentage =
                    Game.currentPercentage game
                }
      , View.Ui.button []
            { onPress = Just args.onChallenge
            , label = "Challenge your Opponent"
            }
            |> Layout.el [ Style.justifyContentCenter ]
      ]
        |> Layout.column [ Style.gap "16px" ]
    ]
        |> Layout.column
            [ Style.height "100%"
            , Layout.contentWithSpaceBetween
            ]
