module Game exposing (..)

import Action exposing (Action(..))
import Card exposing (Card, CardId, CardType)
import Dict exposing (Dict)
import Random exposing (Generator)
import Set exposing (Set)


type alias Game =
    { cards : { dict : Dict CardId Card, nextId : CardId }
    , drawPile : List CardId
    , discardPile : List CardId
    , hand : Dict CardType (List CardId)
    , graveyard : List CardId
    }


init : Game
init =
    { cards =
        { dict =
            [ List.range 0 3 |> List.map (\id -> ( id, Card.Food ))
            , List.range 4 6 |> List.map (\id -> ( id, Card.Stone ))
            , List.range 7 9 |> List.map (\id -> ( id, Card.Wood ))
            ]
                |> List.concat
                |> Dict.fromList
        , nextId = 10
        }
    , drawPile = List.range 0 9
    , discardPile = []
    , hand = Dict.empty
    , graveyard = []
    }


getCardFrom : Game -> CardId -> Maybe ( CardId, Card )
getCardFrom game cardId =
    game.cards.dict
        |> Dict.get cardId
        |> Maybe.map (Tuple.pair cardId)


drawCards : Int -> Game -> Game
drawCards n game =
    { game
        | hand =
            List.take n game.drawPile
                |> List.filterMap (getCardFrom game)
                |> List.foldl
                    (\( cardId, card ) ->
                        Dict.update (Card.cardType card)
                            (\maybe ->
                                maybe
                                    |> Maybe.withDefault []
                                    |> (::) cardId
                                    |> Just
                            )
                    )
                    game.hand
        , drawPile = game.drawPile |> List.drop n
    }


shuffle : Game -> Generator Game
shuffle game =
    game.discardPile
        ++ game.drawPile
        |> (\l ->
                Random.list (List.length l) (Random.float 0 1)
                    |> Random.map
                        (\randomList ->
                            { game
                                | drawPile =
                                    List.map2 Tuple.pair randomList l
                                        |> List.sortBy Tuple.first
                                        |> List.map Tuple.second
                                , discardPile = []
                            }
                        )
           )


discardAllCards : Game -> Game
discardAllCards game =
    { game
        | hand = Dict.empty
        , discardPile =
            Dict.values game.hand
                |> List.concat
                |> (++) game.discardPile
    }


removeCardsFromHand : Set CardId -> Game -> Game
removeCardsFromHand set game =
    { game | hand = game.hand |> Dict.map (\_ -> List.filter (\card -> Set.member card set |> not)) }


addCardToDiscardPile : Card -> Game -> Game
addCardToDiscardPile card game =
    { game
        | cards =
            game.cards
                |> (\c ->
                        { c
                            | dict = c.dict |> Dict.insert c.nextId card
                            , nextId = c.nextId + 1
                        }
                   )
        , discardPile = game.cards.nextId :: game.discardPile
    }


applyAction : Action -> Game -> Generator ( Game, List Action )
applyAction action game =
    case action of
        AddCardToDiscardPile card ->
            ( addCardToDiscardPile card game, [] ) |> Random.constant

        DiscardAllCards ->
            ( discardAllCards game, [] ) |> Random.constant

        RemoveCards cards ->
            ( removeCardsFromHand (Set.fromList cards) game, [] ) |> Random.constant

        DrawCards n ->
            if List.length game.drawPile >= n then
                ( drawCards n game, [] ) |> Random.constant

            else if List.length game.drawPile + List.length game.discardPile >= n then
                if game.drawPile == [] then
                    shuffle game
                        |> Random.map (\it -> ( it, [ Action.DrawCards (n - List.length game.drawPile) ] ))

                else
                    ( drawCards n game
                    , [ Action.Shuffle, Action.DrawCards (n - List.length game.drawPile) ]
                    )
                        |> Random.constant

            else
                ( drawCards n game, [] ) |> Random.constant

        Shuffle ->
            shuffle game |> Random.map (\it -> ( it, [] ))
