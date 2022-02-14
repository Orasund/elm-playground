module Depp.View.Card exposing (toString, view, withActions)

import Cards exposing (Face(..), Suit(..))
import Depp.Data.Deck as Deck exposing (Card)
import Depp.Data.Game as Game exposing (Action, Game)
import Depp.View as View
import Html exposing (Html)
import List.Extra as List
import UndoList.Decode exposing (msg)


toString : Game -> Card -> String
toString game card =
    Deck.suitToString card.suit ++ (card |> Game.value game |> String.fromInt)


view : Game -> Card -> Html msg
view game card =
    card
        |> toString game
        |> Html.text
        |> List.singleton
        |> Html.div []


withActions : List { label : String, onClick : Maybe msg } -> Game -> Card -> Html msg
withActions list game card =
    list
        |> View.actionGroup (toString game card)
        |> List.singleton
        |> Html.p []
        |> List.singleton
        |> Html.aside []
        |> List.singleton
        |> Html.article []
