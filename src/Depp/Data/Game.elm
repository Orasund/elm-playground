module Depp.Data.Game exposing (Action(..), Game, actions, isWon, new, play, value)

import Array exposing (Array)
import Cards exposing (Face(..), Suit(..))
import Depp.Config as Config
import Depp.Data.Deck as Deck exposing (Card)
import Depp.Data.Rule as Rule exposing (ClubsRule(..), DiamondsRule(..), HeartsRule(..), Rule(..))
import Dict.Any as AnyDict exposing (AnyDict)
import Emojidojo.String exposing (game)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Random.List
import Set.Any as AnySet exposing (AnySet)
import Time exposing (Month(..))


type alias Game =
    { drawPile : List Card
    , board : AnySet ( Int, Int ) Card
    , hand : AnySet ( Int, Int ) Card
    , rules : AnyDict String Rule Suit
    }


type Action
    = PlayCard { hand : Card, board : Card }
    | SwapCards { hand : Card, board : Card }
    | Redraw Card


new : Generator Game
new =
    Deck.new
        |> Random.map
            (\deck ->
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
                }
            )


{-| We assume the action has been validated before
-}
play : Action -> Game -> Generator Game
play action game =
    case action of
        PlayCard { hand, board } ->
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
            in
            { game
                | hand =
                    game.hand
                        |> (if
                                game.rules
                                    |> AnyDict.get (ClubsRule HighestStaysInHand)
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
                            then
                                identity

                            else
                                AnySet.remove hand
                           )
                        |> maybeAdd cHand
                , board =
                    game.board
                        |> AnySet.remove board
                        |> maybeAdd cBoard
                , drawPile =
                    drawPile
            }
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


value : Game -> Card -> Int
value game { suit, face } =
    if
        game.rules
            |> AnyDict.get (HeartsRule HaveSameValue)
            |> Maybe.map ((==) suit)
            |> Maybe.withDefault False
    then
        Config.heartsRuleSameValue

    else if face == Ace then
        11

    else
        face
            |> Deck.faceToInt
            |> (+) 1
            |> min 10


isBiggerThen : Card -> Game -> Card -> Bool
isBiggerThen c2 game c1 =
    if c1.suit == c2.suit then
        value game c1 >= value game c2

    else
        Deck.trump == c1.suit


internalIsValid : Game -> Action -> Bool
internalIsValid game action =
    case action of
        PlayCard args ->
            args.hand |> isBiggerThen args.board game

        SwapCards args ->
            game.rules
                |> AnyDict.get (DiamondsRule MaySwapWithSameValue)
                |> Maybe.map
                    (\suit ->
                        (args.hand.suit == suit || args.board.suit == suit)
                            && (value game args.hand == value game args.board)
                    )
                |> Maybe.withDefault False

        Redraw card ->
            AnySet.member card game.hand
                && (AnySet.size game.hand > 2)
