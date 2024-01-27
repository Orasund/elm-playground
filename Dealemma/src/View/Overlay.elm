module View.Overlay exposing (..)

import Game exposing (Game)
import Goal
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Suit exposing (Suit(..))
import View.Card
import View.Goal
import View.Ui


gameEnd : { yourTurn : Bool, onNextRound : msg } -> Game -> Html msg
gameEnd args game =
    let
        bet =
            game.playedCards
                |> List.head
                |> Maybe.withDefault { suit = Heart, goal = [] }
    in
    [ (if args.yourTurn then
        "You challenge your opponent for "

       else
        "Your opponent challenges you for "
      )
        ++ (Goal.probability bet.goal
                |> String.fromInt
           )
        ++ " CREDITS"
        |> Layout.text
            [ Style.justifyContentCenter
            ]
    , (if args.yourTurn then
        "Opponents "

       else
        "Your "
      )
        ++ "Challenge"
        |> Layout.text
            [ Style.justifyContentCenter
            , Html.Attributes.style "font-size" "24px"
            ]
    , View.Goal.toHtml [] { big = True } bet.goal
    , "Cards in game"
        |> Layout.text
            [ Html.Attributes.style "font-size" "24px"
            , Style.justifyContentCenter
            ]
    , game.yourCards
        ++ game.opponentCards
        ++ game.playedCards
        |> List.sortBy (\card -> Suit.icon card.suit)
        |> List.map
            (\card ->
                card.suit
                    |> Just
                    |> View.Goal.viewSuit []
                        { big = True }
            )
        |> Layout.row [ Style.gap "4px" ]
    , (if
        Game.isWon game
            |> (if args.yourTurn then
                    not

                else
                    identity
               )
       then
        "You Win"

       else
        "You Loose"
      )
        |> Layout.text
            [ Html.Attributes.style "font-size" "64px"
            , Style.justifyContentCenter
            ]
    , View.Ui.button []
        { label = "Next Round"
        , onPress = Just args.onNextRound
        }
        |> Layout.el [ Style.justifyContentCenter ]
    ]
        |> Layout.column
            [ Layout.contentWithSpaceBetween
            , Style.height "100%"
            ]
