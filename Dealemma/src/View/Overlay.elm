module View.Overlay exposing (..)

import Card exposing (Card)
import Config
import Dict exposing (Dict)
import Game exposing (CardId, Game)
import Game.Area
import Game.Entity
import Goal exposing (Category(..))
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
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
    , probabilities : Dict String Int
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
                    |> Layout.row
                        [ Style.gap "4px"
                        , Style.justifyContentCenter
                        ]
            )
        |> Layout.column
            [ Style.justifyContentCenter
            , Style.gap "4px"
            ]
    , "Pick a card to add to the deck"
        |> Layout.text
            [ Style.justifyContentCenter
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
                    { probability =
                        args.probabilities
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
            [ Style.height (String.fromInt View.Card.height ++ "px")
            , Style.justifyContentCenter
            ]
    ]
        |> Layout.column [ Layout.contentWithSpaceBetween ]


tutorial : { onNext : Int -> msg, page : Int } -> Html msg
tutorial args =
    (case args.page of
        0 ->
            [ "There are 16 cards in the deck"
                |> Layout.text []
            , Suit.asList
                |> List.map
                    (\suit ->
                        suit
                            |> Just
                            |> View.Goal.viewSuit [] { big = True }
                            |> List.repeat Config.cardsPerSuit
                            |> Layout.row [ Style.gap "4px", Style.justifyContentCenter ]
                    )
                |> Layout.column
                    [ Style.gap "4px"
                    , Style.justifyContentCenter
                    ]
            , "Each round both players draw 4 cards" |> Layout.text []
            , View.Goal.viewSuits { big = True } 4 Nothing
                |> List.repeat 2
                |> Layout.row [ Layout.contentWithSpaceBetween ]
            , [ View.Ui.button []
                    { label = "Next"
                    , onPress = Just (args.onNext (args.page + 1))
                    }
              ]
                |> Layout.row [ Layout.contentCentered ]
            ]

        1 ->
            let
                suit =
                    Heart

                goal =
                    [ ThreeOfAKind, PairOf suit ]
            in
            [ "A card consists of a value, a suit and a bet"
                |> Layout.text []
            , [ ( -1, { suit = suit, goal = goal } )
                    |> View.Card.toHtml []
                        { probability = 42
                        , faceUp = True
                        , active = False
                        }
                    |> Game.Entity.map Tuple.second
                    |> Game.Entity.toHtml []
              , Layout.column
                    [ Style.positionAbsolute
                    , Style.left "-100px"
                    , Style.top "-8px"
                    , Html.Attributes.style "text-align" "right"
                    , Style.alignItemsFlexEnd
                    ]
                    [ "Value: " |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
                    , String.fromInt 42
                        ++ " CREDITS"
                        |> Layout.text []
                    ]
              , Html.div
                    [ Style.positionAbsolute
                    , Style.right "-50px"
                    , Style.top "8px"
                    ]
                    [ "suit: " |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
                    , Suit.icon suit |> Layout.text []
                    ]
              , Html.div
                    [ Style.positionAbsolute
                    , Style.left "-5px"
                    , Style.top "130px"
                    , Style.width "100px"
                    ]
                    [ "bet: " |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
                    , View.Goal.toHtml []
                        { big = False }
                        goal
                    , Goal.description goal
                        |> Layout.text []
                    ]
              ]
                |> Html.div
                    [ Style.positionRelative
                    ]
                |> Layout.el
                    [ Style.justifyContentCenter
                    , Style.height "200px"
                    ]
            , [ "By playing this card, im saying:" |> Layout.text []
              , "\"I bet 42 CREDITS that there are at least three of a kind and two hearts in the game\"" |> Layout.text [ Html.Attributes.style "font-weight" "bold" ]
              ]
                |> Layout.column [ Layout.gap 8 ]
            , View.Ui.button []
                { label = "Next"
                , onPress = Just (args.onNext (args.page + 1))
                }
                |> Layout.el [ Style.justifyContentCenter ]
            ]

        2 ->
            [ "Each turn a player can either play a card with a lower value or challenge the bet (calling the bluff)."
                |> Layout.text []
            , Random.initialSeed 42
                |> Random.step
                    (Goal.asList
                        |> Card.newDeck
                        |> Random.andThen Game.fromDeck
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
                |> Layout.text
                    [ Html.Attributes.style "font-size" "24px"
                    , Style.justifyContentCenter
                    ]
            , "The game ends once you have no CREDITS left" |> Layout.text []
            , "You start with 100 CREDITS"
                |> Layout.text
                    [ Style.justifyContentCenter
                    ]
            , View.Ui.button []
                { label = "Start Game"
                , onPress = Just (args.onNext (args.page + 1))
                }
                |> Layout.el [ Style.justifyContentCenter ]
            ]
    )
        |> Layout.column [ Layout.contentWithSpaceBetween ]


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
        ++ (game.probabilities
                |> Dict.get (Goal.description bet.goal)
                |> Maybe.withDefault 0
                |> String.fromInt
           )
        ++ " CREDITS"
        |> Layout.text
            [ Style.justifyContentCenter
            ]
    , [ (if args.yourTurn then
            "Their "

         else
            "Your "
        )
            ++ "Call"
            |> Layout.text
                [ Style.justifyContentCenter
                , Html.Attributes.style "font-size" "24px"
                ]
      , View.Goal.toHtml [] { big = True } bet.goal
      ]
        |> Layout.column [ Style.gap "8px" ]
    , [ "Cards in game"
            |> Layout.text
                [ Html.Attributes.style "font-size" "24px"
                , Style.justifyContentCenter
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
            |> Layout.row [ Style.gap "4px" ]
      ]
        |> Layout.column [ Style.gap "8px" ]
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
        |> Layout.text
            [ Html.Attributes.style "font-size" "64px"
            , Style.justifyContentCenter
            ]
    , View.Ui.button []
        { label = "Next Round"
        , onPress = Just args.onNextRound
        }
        |> Layout.el [ Style.justifyContentCenter ]
    ]
        |> Layout.column
            [ Layout.contentWithSpaceBetween
            , Style.height "100%"
            ]
