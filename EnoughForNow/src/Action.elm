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
    | InternalMoveFromAreaToDiscardPile


fromCard : Card -> List Action
fromCard card =
    case card of
        Wood ->
            []

        Stone ->
            [ FilterHandAndThen ((==) Wood)
                (\list ->
                    DiscardCards list
                        :: List.map (\_ -> AddCardToArea Food) list
                        ++ [ InternalMoveFromAreaToDiscardPile ]
                )
            ]

        Food ->
            [ AddCardToArea Wood
            , InternalMoveFromAreaToDiscardPile
            , AddCardToArea Stone
            , InternalMoveFromAreaToDiscardPile
            , DiscardAllCards
            , Shuffle
            , DrawCards Config.cardsInHand
            ]
