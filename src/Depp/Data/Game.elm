module Depp.Data.Game exposing (Action(..), Card, Game, actions, isWon, new, play, value)

import Array exposing (Array)
import Cards exposing (Face(..), Suit(..))
import Depp.Config as Config
import Depp.Data.Deck as Deck
import Emojidojo.String exposing (game)
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Random.List
import Set.Any as AnySet exposing (AnySet)


type alias Card =
    ( Suit, Face )


type alias Game =
    { drawPile : List Card
    , board : AnySet ( Int, Int ) Card
    , hand : AnySet ( Int, Int ) Card
    }


type Action
    = PlayCard { hand : Card, board : Card }
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
                , board = board |> AnySet.fromList (Tuple.mapBoth Deck.suitToInt Deck.faceToInt)
                , hand = hand |> AnySet.fromList (Tuple.mapBoth Deck.suitToInt Deck.faceToInt)
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
                        |> AnySet.remove hand
                        |> maybeAdd cHand
                , board =
                    game.board
                        |> AnySet.remove board
                        |> maybeAdd cBoard
                , drawPile =
                    drawPile
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
                            | hand = card :: hand |> AnySet.fromList (Tuple.mapBoth Deck.suitToInt Deck.faceToInt)
                            , drawPile = drawPile
                            , board = board |> AnySet.fromList (Tuple.mapBoth Deck.suitToInt Deck.faceToInt)
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
                                            |> PlayCard
                                    )
                        )
               )
            |> List.filter (internalIsValid game)


value : Card -> Int
value ( suit, face ) =
    if face == Ace || Deck.isTrump suit then
        11

    else
        face
            |> Deck.faceToInt
            |> (+) 1
            |> min 10


isBiggerThen : Card -> Card -> Bool
isBiggerThen (( s2, f2 ) as c2) (( s1, f1 ) as c1) =
    if s1 == s2 then
        value c1 >= value c2

    else
        Deck.isTrump s1


internalIsValid : Game -> Action -> Bool
internalIsValid game action =
    case action of
        PlayCard args ->
            args.hand |> isBiggerThen args.board

        Redraw card ->
            AnySet.member card game.hand
                && (AnySet.size game.hand > 1)
