module Mezzo.Data.Game exposing (Game, discard, init, play)

import Array exposing (Array)
import Array.Extra as Array
import List.Extra as List
import Mezzo.Data.Card as Card exposing (Card, CardPart, CardSort(..), Suit)
import Queue exposing (Queue)
import Random exposing (Generator)
import Random.List as Random


type alias Game =
    { deck : Queue CardPart
    , hand : Array Card
    , card : Card
    }


initDeck : List CardPart
initDeck =
    [ [ { suit = Card.black
        , sort = Valued 0
        }
      ]
    , [ Card.red, Card.blue ]
        |> List.concatMap
            (\suit ->
                List.range 1 2
                    |> List.map
                        (\value ->
                            { suit = suit
                            , sort = Valued value
                            }
                        )
                    |> List.repeat 3
                    |> List.concat
            )
    ]
        |> List.concat


init : Generator Game
init =
    initDeck
        |> Random.shuffle
        |> Random.map
            (\l0 ->
                let
                    defaultCard =
                        { suit = Card.white
                        , sort = Valued 0
                        }

                    ( card, l1 ) =
                        l0
                            |> List.uncons
                            |> Maybe.withDefault ( defaultCard, [] )

                    ( hand, deck ) =
                        l1
                            |> List.splitAt 3
                in
                { deck =
                    (if deck |> List.length |> modBy 2 |> (==) 0 then
                        defaultCard
                            :: deck

                     else
                        deck
                    )
                        |> Queue.fromList
                , hand =
                    hand
                        |> List.map Card.fromPart
                        |> Array.fromList
                , card = card |> Card.fromPart
                }
            )



--------------------------------------------------------------------------------
-- play
--------------------------------------------------------------------------------


playCard : Int -> Game -> Result () Game
playCard i game =
    game.hand
        |> Array.get i
        |> Maybe.map
            (\card ->
                let
                    ( p1, p2 ) =
                        game.card
                            |> Card.toParts
                in
                { game
                    | card =
                        card
                    , deck =
                        game.deck
                            |> Queue.enqueue p1
                            |> Queue.enqueue p2
                    , hand =
                        game.hand
                            |> Array.removeAt i
                }
                    |> Ok
            )
        |> Maybe.withDefault (Err ())


discardCard : Int -> Game -> Game
discardCard i game =
    { game
        | hand =
            game.hand
                |> Array.removeAt i
    }


drawCard : Game -> Result () Game
drawCard game =
    case game.deck |> Queue.dequeue of
        ( Just p1, tail ) ->
            case tail |> Queue.dequeue of
                ( Just p2, deck ) ->
                    { game
                        | deck = deck
                        , hand =
                            game.hand
                                |> Array.push (Card.fromParts ( p1, p2 ))
                    }
                        |> Ok

                ( Nothing, queue ) ->
                    Err ()

        ( Nothing, tail ) ->
            Err ()


addCard : Card -> Game -> Game
addCard card game =
    let
        ( p1, p2 ) =
            card
                |> Card.toParts
    in
    { game
        | deck =
            game.deck
                |> Queue.enqueue p1
                |> Queue.enqueue p2
    }


validMove : Int -> Game -> Bool
validMove i game =
    game.hand
        |> Array.get i
        |> Maybe.map (Card.validAfter game.card)
        |> Maybe.withDefault False


play : Int -> Game -> Result () Game
play i g =
    case g.hand |> Array.get i of
        Just card ->
            if validMove i g then
                g
                    |> playCard i
                    |> Result.map
                        (case card.sort of
                            Valued _ ->
                                drawCard

                            Add ->
                                addCard { card | sort = Valued 0 }
                                    >> drawCard
                        )
                    |> Result.withDefault (Ok g)

            else
                Ok g

        Nothing ->
            Ok g


discard : Int -> Game -> Result () Game
discard i g =
    case g.hand |> Array.get i of
        Just card ->
            g
                |> discardCard i
                |> (case card.sort of
                        Valued _ ->
                            drawCard

                        Add ->
                            addCard { card | sort = Valued 0 }
                                >> drawCard
                   )

        Nothing ->
            Ok g
