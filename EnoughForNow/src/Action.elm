module Action exposing (..)

import Card exposing (Card(..), CardId)
import Config


type Action
    = DrawCards Int
    | MoveToArea (List CardId)
    | DiscardCards (List CardId)
    | DiscardAllCards
    | Shuffle
    | AddCardToArea Card
    | FilterHandAndThen (Card -> Bool) (List CardId -> List Action)
    | ClearArea
    | MoveFromAreaToDiscardPile
    | AddScoreAndThen Action
    | Wait


fromCard : Card -> List Action
fromCard card =
    case card of
        Wood ->
            [ DrawCards 4
            ]

        Fire ->
            [ FilterHandAndThen ((==) Wood)
                (\list ->
                    DiscardCards list
                        :: List.map (\_ -> AddCardToArea Food) list
                )
            , MoveFromAreaToDiscardPile
            , Shuffle
            ]

        Food ->
            [ AddScoreAndThen (AddCardToArea Wood)
            , AddCardToArea Fire
            , AddCardToArea Fear
            , MoveFromAreaToDiscardPile
            , DiscardAllCards
            , Shuffle
            , DrawCards Config.cardsInHand
            ]

        Fear ->
            [ FilterHandAndThen ((==) Food)
                (\list ->
                    [ DiscardCards list
                    ]
                )
            , AddCardToArea Food
            , MoveFromAreaToDiscardPile
            , Shuffle
            , DrawCards 2
            ]


description : Card -> String
description card =
    case card of
        Wood ->
            "Draw 4 cards."

        Fire ->
            "Discard all your wood. Add 1 Food to your deck for each wood you discarded this way."

        Food ->
            "Add 1 Wood, 1 Fire and 1 Fear to your deck. Shuffle your hand into the deck and redraw 7 cards."

        Fear ->
            "Discard all your food. Add 1 Food. Draw 2 cards."
