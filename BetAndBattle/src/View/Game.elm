module View.Game exposing (..)

import Game exposing (Card, Game)
import Html exposing (Html)
import Html.Style as Style
import Layout
import View.Card
import View.Hand


toHtml :
    { onChallenge : msg
    , onPlay : Card -> msg
    , yourTurn : Bool
    }
    -> Game
    -> Html msg
toHtml args game =
    [ game.opponentCards
        |> View.Hand.opponent
    , case game.playedCards of
        head :: tail ->
            [ View.Card.toHtml [ Style.height "200px" ] head
            , tail
                |> List.map View.Card.small
                |> Layout.row [ Style.gap "8px" ]
            ]
                |> Layout.row [ Style.gap "16px" ]

        [] ->
            Layout.el [] Layout.none
    , [ (if not args.yourTurn then
            "Waiting..."

         else
            "Play a card or challenge the played card"
        )
            |> Layout.text []
      , game.yourCards
            |> View.Hand.toHtml
                { onPlay = args.onPlay
                , currentPercentage =
                    Game.currentPercentage game
                }
      , Layout.textButton []
            { label = "Challenge"
            , onPress = Just args.onChallenge
            }
      ]
        |> Layout.column [ Style.gap "16px" ]
    ]
        |> Layout.column
            [ Style.height "100%"
            , Layout.contentWithSpaceBetween
            ]
