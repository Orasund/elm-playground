module View.Overlay exposing (..)

import Game exposing (Game)
import Goal
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
import Layout
import View.Card


gameEnd : { yourTurn : Bool } -> Game -> Html msg
gameEnd args game =
    [ game.yourCards
        ++ game.opponentCards
        ++ game.playedCards
        |> List.map View.Card.small
        |> Layout.row [ Style.gap "4px" ]
    , game.playedCards
        |> List.head
        |> Maybe.map (View.Card.toHtml [])
        |> Maybe.withDefault (Html.text "No card was played")
        |> Layout.el []
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
        |> Layout.text [ Html.Attributes.style "font-size" "24px" ]
    ]
        |> Layout.column []
