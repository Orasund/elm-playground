module Depp.View.Card exposing (toString, view, withActions)

import Cards exposing (Face(..), Suit(..))
import Depp.Data.Deck as Deck
import Depp.Data.Game as Game exposing (Action, Card)
import Depp.View as View
import Html exposing (Html)
import List.Extra as List
import UndoList.Decode exposing (msg)


toString : Card -> String
toString ( suit, face ) =
    if Deck.isTrump suit then
        "Joker"

    else
        Deck.suitToString suit ++ String.fromInt (Game.value ( suit, face ))



--Deck.suitToString suit ++ Deck.faceToString face


view : Card -> Html msg
view card =
    card
        |> toString
        |> Html.text
        |> List.singleton
        |> Html.div []


withActions : List { label : String, onClick : Maybe msg } -> Card -> Html msg
withActions list card =
    list
        |> View.actionGroup (toString card)
        |> List.singleton
        |> Html.p []
        |> List.singleton
        |> Html.aside []
        |> List.singleton
        |> Html.article []
