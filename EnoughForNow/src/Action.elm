module Action exposing (..)

import Card exposing (Card(..), CardId)
import Config
import Set exposing (Set)


type Action
    = DrawCards Int
    | RemoveCards (List CardId)
    | DiscardAllCards
    | Shuffle
    | AddCardToDiscardPile Card


fromCard : Card -> List Action
fromCard card =
    case card of
        Wood ->
            []

        Stone ->
            []

        Food ->
            [ AddCardToDiscardPile Wood
            , AddCardToDiscardPile Stone
            , DiscardAllCards
            , Shuffle
            , DrawCards Config.cardsInHand
            ]
