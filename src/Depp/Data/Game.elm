module Depp.Data.Game exposing (Action(..), Game, actions, isWon, new, play, suitToString, updateValue, value)

import Array exposing (Array)
import Cards exposing (Face(..), Suit(..))
import Depp.Config as Config
import Depp.Data.Deck as Deck exposing (Card)
import Depp.Data.Rule as Rule exposing (Rule(..))
import Dict.Any as AnyDict exposing (AnyDict)
import Emojidojo.String exposing (game)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Random.List
import Set.Any as AnySet exposing (AnySet)
import Time exposing (Month(..))


type alias CardState =
    { value : Int }


type alias Game =
    { drawPile : List Card
    , board : AnySet ( Int, Int ) Card
    , hand : AnySet ( Int, Int ) Card
    , rules : AnyDict Int Rule Suit
    , cardStates : AnyDict ( Int, Int ) Card CardState
    , trump : Suit
    }


type Action
    = PlayCard { hand : Card, board : Card }
    | SwapCards { hand : Card, board : Card }
    | Redraw Card


new : Generator Game
new =
    Random.map2
        (\deck trump ->
            let
                ( board, ( hand, drawPile ) ) =
                    deck
                        |> List.splitAt Config.cardAmountOnBoard
                        |> Tuple.mapSecond (List.splitAt Config.cardAmountInHand)
            in
            { drawPile = drawPile
            , board = board |> AnySet.fromList Deck.cardToComparable
            , hand = hand |> AnySet.fromList Deck.cardToComparable
            , rules = Rule.defaultRules
            , cardStates = AnyDict.empty Deck.cardToComparable
            , trump = trump
            }
        )
        Deck.new
        (case Array.toList Deck.suits of
            head :: tail ->
                Random.uniform head tail

            [] ->
                Random.constant Hearts
        )


suitToString : Suit -> Game -> String
suitToString suit game =
    if suit == game.trump then
        "☆"

    else
        case suit of
            Spades ->
                "♠"

            Diamonds ->
                "♦"

            Clubs ->
                "♣"

            Hearts ->
                "♥"


playCard : { hand : Card, board : Card } -> Game -> Game
playCard { hand, board } game =
    let
        ( cHand, ( cBoard, drawPile ) ) =
            game.drawPile
                |> List.splitAt 1
                |> Tuple.mapSecond (List.splitAt 1)

        maybeAdd c =
            c
                |> List.head
                |> Maybe.map AnySet.insert
                |> Maybe.withDefault identity

        highestStaysInHandRuleApplies =
            game.rules
                |> AnyDict.get HighestStaysInHand
                |> Maybe.map
                    (\suit ->
                        (hand.suit == suit)
                            && (game.hand
                                    |> AnySet.toList
                                    |> List.filterMap
                                        (\card ->
                                            if card.suit == suit then
                                                card
                                                    |> value game
                                                    |> Just

                                            else
                                                Nothing
                                        )
                                    |> List.maximum
                                    |> (==) (hand |> value game |> Just)
                               )
                    )
                |> Maybe.withDefault False

        hasHigherValue =
            value game hand >= value game board
    in
    { game
        | hand =
            game.hand
                |> (if highestStaysInHandRuleApplies then
                        identity

                    else
                        AnySet.remove hand
                   )
                |> maybeAdd cHand
        , drawPile =
            drawPile
    }
        --in the default case the player has a higher value
        --only in a special rule (ReduceValueIfLower) the value is acutally lower.
        |> (if hasHigherValue then
                \g ->
                    { g
                        | board =
                            g.board
                                |> AnySet.remove board
                                |> maybeAdd cBoard
                    }

            else
                updateValue ((+) -(value game hand)) board
           )


{-| We assume the action has been validated before
-}
play : Action -> Game -> Generator Game
play action game =
    case action of
        PlayCard args ->
            game
                |> playCard args
                |> Random.constant

        SwapCards { hand, board } ->
            { game
                | hand =
                    game.hand
                        |> AnySet.remove hand
                        |> AnySet.insert board
                , board =
                    game.board
                        |> AnySet.remove board
                        |> AnySet.insert hand
            }
                |> Random.constant

        Redraw card ->
            let
                newHandSize =
                    game.hand
                        |> AnySet.size
                        |> (+) -2
            in
            game.drawPile
                ++ (game.hand |> AnySet.remove card |> AnySet.toList)
                ++ (game.board |> AnySet.toList)
                |> Random.List.shuffle
                |> Random.map
                    (\list ->
                        let
                            ( hand, ( board, drawPile ) ) =
                                list
                                    |> List.splitAt newHandSize
                                    |> Tuple.mapSecond (List.splitAt Config.cardAmountOnBoard)
                        in
                        { game
                            | hand = card :: hand |> AnySet.fromList Deck.cardToComparable
                            , drawPile = drawPile
                            , board = board |> AnySet.fromList Deck.cardToComparable
                        }
                    )


isWon : Game -> Bool
isWon game =
    game.board |> AnySet.size |> (==) 0


actions : Game -> List Action
actions game =
    if isWon game then
        []

    else
        (game.hand
            |> AnySet.toList
            |> List.map Redraw
        )
            ++ (game.hand
                    |> AnySet.toList
                    |> List.concatMap
                        (\handC ->
                            game.board
                                |> AnySet.toList
                                |> List.map
                                    (\boardC ->
                                        { hand = handC, board = boardC }
                                    )
                                |> List.concatMap
                                    (\args ->
                                        [ PlayCard args, SwapCards args ]
                                    )
                        )
               )
            |> List.filter (internalIsValid game)


updateValue : (Int -> Int) -> Card -> Game -> Game
updateValue fun card game =
    { game
        | cardStates =
            game.cardStates
                |> AnyDict.update card
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault { value = value game card }
                            |> (\state -> { state | value = fun state.value })
                            |> Just
                    )
    }


value : Game -> Card -> Int
value game card =
    case game.cardStates |> AnyDict.get card of
        Just state ->
            state.value

        Nothing ->
            if
                game.rules
                    |> AnyDict.get HaveSameValue
                    |> Maybe.map ((==) card.suit)
                    |> Maybe.withDefault False
            then
                Config.heartsRuleSameValue

            else if card.face == Ace then
                Config.aceValue

            else
                let
                    val =
                        card.face
                            |> Deck.faceToInt
                            |> (+) 1
                in
                if val > 10 then
                    Config.courtCardValue

                else
                    val


isBiggerThen : Card -> Game -> Card -> Bool
isBiggerThen c2 game c1 =
    if c1.suit == c2.suit then
        value game c1 >= value game c2

    else
        game.trump == c1.suit


internalIsValid : Game -> Action -> Bool
internalIsValid game action =
    case action of
        PlayCard args ->
            (game.rules
                |> AnyDict.get ReduceValueIfLower
                |> Maybe.map
                    (\suit ->
                        (args.hand.suit == suit)
                            && ((args.hand.suit == args.board.suit)
                                    || (game.trump == args.hand.suit)
                               )
                    )
                |> Maybe.withDefault False
            )
                || (args.hand |> isBiggerThen args.board game)

        SwapCards args ->
            game.rules
                |> AnyDict.get MaySwapWithSameValue
                |> Maybe.map
                    (\suit ->
                        (args.hand.suit == suit || args.board.suit == suit)
                            && (value game args.hand == value game args.board)
                    )
                |> Maybe.withDefault False

        Redraw card ->
            AnySet.member card game.hand
                && (AnySet.size game.hand > 2)
