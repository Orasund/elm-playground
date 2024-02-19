module View.Overlay exposing (..)

import Card exposing (Card)
import Config
import Dict exposing (Dict)
import Game exposing (Game)
import Game.Area
import Game.Entity
import Goal exposing (Category(..))
import Html exposing (Html)
import Html.Style
import Layout
import List.Extra
import Random
import Set
import Suit exposing (Suit(..))
import View.Card
import View.Game
import View.Goal
import View.Ui


shop :
    { onChoose : Card -> msg
    , deck : List Card
    , values : Dict String Int
    }
    -> List Card
    -> Html msg
shop args list =
    [ args.deck
        |> List.Extra.gatherEqualsBy .suit
        |> List.sortBy (\( _, l ) -> List.length l)
        |> List.map
            (\( card, l ) ->
                card.suit
                    |> Just
                    |> View.Goal.viewSuit []
                        { big = True }
                    |> List.repeat (List.length l + 1)
                    |> Html.div
                        [ Html.Style.displayFlex
                        , Html.Style.flexDirectionRow
                        , Html.Style.gapPx 4
                        , Html.Style.justifyContentCenter
                        ]
            )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.justifyContentCenter
            , Html.Style.gapPx 4
            ]
    , "Pick a card to add to the deck"
        |> Layout.divText
            [ Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            ]
    , list
        |> List.indexedMap
            (\i card ->
                View.Card.toHtml
                    (Layout.asButton
                        { label = "Pick"
                        , onPress = Just (args.onChoose card)
                        }
                    )
                    { value =
                        args.values
                            |> Dict.get (Goal.description card.goal)
                            |> Maybe.withDefault 0
                    , faceUp = True
                    , active = True
                    }
                    ( -i, card )
            )
        |> Game.Area.withPolarPosition
            { minAngle = 0
            , maxAngle = 0
            , minDistance = -4 - toFloat View.Card.height * 2 / 6
            , maxDistance = 4 + toFloat View.Card.height * 2 / 6
            }
        |> Game.Area.toHtml
            [ Html.Style.heightPx View.Card.height
            , Html.Style.justifyContentCenter
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.justifyContentSpaceBetween
            ]


tutorial : { onNext : Int -> msg, page : Int } -> Html msg
tutorial args =
    (case args.page of
        0 ->
            [ "There are 16 cards in the deck"
                |> Layout.divText [ Html.Style.displayFlex ]
            , Suit.asList
                |> List.map
                    (\suit ->
                        suit
                            |> Just
                            |> View.Goal.viewSuit [] { big = True }
                            |> List.repeat Config.cardsPerSuit
                            |> Html.div
                                [ Html.Style.displayFlex
                                , Html.Style.flexDirectionRow
                                , Html.Style.gapPx 4
                                , Html.Style.justifyContentCenter
                                ]
                    )
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gapPx 4
                    , Html.Style.justifyContentCenter
                    ]
            , "Each round both players draw 4 cards" |> Layout.divText []
            , View.Goal.viewSuits { big = True } 4 Nothing
                |> List.repeat 2
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionRow
                    , Html.Style.justifyContentSpaceBetween
                    ]
            , [ View.Ui.button []
                    { label = "Next"
                    , onPress = Just (args.onNext (args.page + 1))
                    }
              ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionRow
                    , Html.Style.justifyContentCenter
                    ]
            ]

        1 ->
            let
                suit =
                    Heart

                goal =
                    [ ThreeOfAKind, PairOf suit ]
            in
            [ "A card consists of a value, a suit and a bet"
                |> Layout.divText [ Html.Style.displayFlex ]
            , [ ( -1, { suit = suit, goal = goal } )
                    |> View.Card.toHtml []
                        { value = 42
                        , faceUp = True
                        , active = False
                        }
                    |> Game.Entity.map Tuple.second
                    |> Game.Entity.toHtml []
              , Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.positionAbsolute
                    , Html.Style.leftPx -100
                    , Html.Style.topPx -8
                    , Html.Style.textAlignRight
                    , Html.Style.alignItemsFlexEnd
                    ]
                    [ "Value: "
                        |> Layout.divText
                            [ Html.Style.displayFlex
                            , Html.Style.fontWeightBold
                            ]
                    , String.fromInt 42
                        ++ " CREDITS"
                        |> Layout.divText [ Html.Style.displayFlex ]
                    ]
              , Html.div
                    [ Html.Style.positionAbsolute
                    , Html.Style.rightPx -50
                    , Html.Style.topPx 8
                    ]
                    [ "suit: "
                        |> Layout.divText
                            [ Html.Style.displayFlex
                            , Html.Style.fontWeightBold
                            ]
                    , Suit.icon suit |> Layout.divText [ Html.Style.displayFlex ]
                    ]
              , Html.div
                    [ Html.Style.positionAbsolute
                    , Html.Style.leftPx -5
                    , Html.Style.topPx 130
                    , Html.Style.widthPx 100
                    ]
                    [ "bet: "
                        |> Layout.divText
                            [ Html.Style.displayFlex
                            , Html.Style.fontWeightBold
                            ]
                    , View.Goal.toHtml []
                        { big = False }
                        goal
                    , Goal.description goal
                        |> Layout.divText [ Html.Style.displayFlex ]
                    ]
              ]
                |> Html.div
                    [ Html.Style.positionRelative
                    ]
                |> Layout.divWrapper
                    [ Html.Style.displayFlex
                    , Html.Style.justifyContentCenter
                    , Html.Style.heightPx 200
                    ]
            , [ "By playing this card, im saying:" |> Layout.divText [ Html.Style.displayFlex ]
              , "\"I bet 42 CREDITS that there are at least three of a kind and two hearts in the game\""
                    |> Layout.divText
                        [ Html.Style.displayFlex
                        , Html.Style.fontWeightBold
                        ]
              ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gapPx 8
                    ]
            , View.Ui.button []
                { label = "Next"
                , onPress = Just (args.onNext (args.page + 1))
                }
                |> Layout.divWrapper
                    [ Html.Style.displayFlex
                    , Html.Style.justifyContentCenter
                    ]
            ]

        2 ->
            [ "Each turn a player can either play a card with a higher value or challenge the bet (calling the bluff)."
                |> Layout.divText [ Html.Style.displayFlex ]
            , Random.initialSeed 42
                |> Random.step
                    (Goal.asList
                        |> Card.newDeck
                        |> Random.andThen (Game.fromDeck 1)
                    )
                |> Tuple.first
                |> View.Game.toHtml
                    { onChallenge = args.onNext (args.page + 1)
                    , onPlay = \_ -> args.onNext (args.page + 1)
                    , yourTurn = True
                    }
            ]

        _ ->
            [ "That's it"
                |> Layout.divText
                    [ Html.Style.fontSizePx 24
                    , Html.Style.justifyContentCenter
                    ]
            , "The game ends once you have no CREDITS left" |> Layout.divText [ Html.Style.displayFlex ]
            , "You start with "
                ++ String.fromInt Config.startingCredits
                ++ " CREDITS"
                |> Layout.divText
                    [ Html.Style.displayFlex
                    , Html.Style.justifyContentCenter
                    ]
            , View.Ui.button []
                { label = "Start Game"
                , onPress = Just (args.onNext (args.page + 1))
                }
                |> Layout.divWrapper [ Html.Style.displayFlex, Html.Style.justifyContentCenter ]
            ]
    )
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.justifyContentSpaceBetween
            ]


gameEnd : { yourTurn : Bool, onNextRound : msg } -> Game -> Html msg
gameEnd args game =
    let
        bet =
            game.playedCards
                |> List.head
                |> Maybe.andThen (Game.getCardFrom game)
                |> Maybe.withDefault { suit = Heart, goal = [] }
    in
    [ (if args.yourTurn then
        "You called the bluff for "

       else
        "They called the bluff for "
      )
        ++ (game.values
                |> Dict.get (Goal.description bet.goal)
                |> Maybe.withDefault 0
                |> String.fromInt
           )
        ++ " CREDITS"
        |> Layout.divText
            [ Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            ]
    , [ (if args.yourTurn then
            "Their "

         else
            "Your "
        )
            ++ "Call"
            |> Layout.divText
                [ Html.Style.displayFlex
                , Html.Style.justifyContentCenter
                , Html.Style.fontSizePx 24
                ]
      , View.Goal.toHtml [] { big = True } bet.goal
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 8
            ]
    , [ "Cards in game"
            |> Layout.divText
                [ Html.Style.fontSizePx 24
                , Html.Style.justifyContentCenter
                ]
      , Set.toList game.yourCards
            ++ Set.toList game.opponentCards
            ++ game.playedCards
            |> List.filterMap (Game.getCardFrom game)
            |> List.sortBy (\card -> Suit.icon card.suit)
            |> List.map
                (\card ->
                    card.suit
                        |> Just
                        |> View.Goal.viewSuit []
                            { big = True }
                )
            |> Html.div
                [ Html.Style.displayFlex
                , Html.Style.flexDirectionRow
                , Html.Style.gapPx 4
                ]
      ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.gapPx 8
            ]
    , (if
        Game.isWon game
            |> (if args.yourTurn then
                    not

                else
                    identity
               )
       then
        "You Win"

       else
        "You Loose"
      )
        |> Layout.divText
            [ Html.Style.displayFlex
            , Html.Style.fontSizePx 64
            , Html.Style.justifyContentCenter
            ]
    , View.Ui.button []
        { label = "Next Round"
        , onPress = Just args.onNextRound
        }
        |> Layout.divWrapper
            [ Html.Style.displayFlex
            , Html.Style.justifyContentCenter
            ]
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.justifyContentSpaceBetween
            , Html.Style.height "100%"
            ]
