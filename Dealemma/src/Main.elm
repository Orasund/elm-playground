module Main exposing (..)

import Browser
import Card exposing (Card)
import Config
import Dict
import Game exposing (CardId, Game)
import Game.Update
import Goal
import Html exposing (Html)
import Html.Attributes
import Html.Style
import Layout
import Process
import Random exposing (Seed)
import Random.List
import Task
import View.Game
import View.Overlay
import View.Ui


type Overlay
    = EndOfRound
    | EndOfGame
    | Tutorial Int
    | Shop (List Card)


type alias Model =
    { game : Game
    , seed : Seed
    , overlay : Maybe Overlay
    , yourTurn : Bool
    , score : Int
    , deck : List Card
    , specialCards : List Card
    , multiplier : Float
    }


type Msg
    = Restart Seed
    | PlayCard CardId
    | ChallengeGoal
    | RequestOpponentTurn
    | EndRoundAndOpenShop Int
    | RequestPageOfTurtorial Int
    | AddCardAndStartNextRound Card


init : () -> ( Model, Cmd Msg )
init () =
    ( restartGame (Random.initialSeed 42)
    , Random.independentSeed
        |> Random.generate Restart
    )


restartGame : Seed -> Model
restartGame seed =
    let
        multiplier =
            1

        rand =
            Card.newDeck Goal.asList
                |> Random.andThen
                    (\d ->
                        d
                            |> Game.fromDeck multiplier
                            |> Random.map
                                (\g ->
                                    { deck = d
                                    , game = g
                                    }
                                )
                    )
                |> Random.andThen
                    (\out ->
                        Random.List.shuffle Card.specialCards
                            |> Random.map
                                (\s ->
                                    { deck = out.deck
                                    , game = out.game
                                    , special = s
                                    }
                                )
                    )

        ( { deck, game, special }, newSeed ) =
            Random.step rand seed
    in
    { game = game
    , seed = newSeed
    , overlay = Just (Tutorial 0)
    , yourTurn = False
    , score = Config.startingCredits
    , deck = deck
    , specialCards = special
    , multiplier = multiplier
    }


view : Model -> Html Msg
view model =
    let
        currentScore =
            model.game.playedCards
                |> List.head
                |> Maybe.andThen
                    (\cardId ->
                        cardId
                            |> Game.getCardFrom model.game
                            |> Maybe.map (Tuple.pair cardId)
                    )
                |> Maybe.andThen
                    (\( _, card ) ->
                        model.game.values
                            |> Dict.get (Goal.description card.goal)
                    )
                |> Maybe.withDefault 0
                |> (*)
                    (if xor (Game.isWon model.game) model.yourTurn then
                        1

                     else
                        -1
                    )
    in
    [ (case model.overlay of
        Just (Shop list) ->
            View.Overlay.shop
                { onChoose = AddCardAndStartNextRound
                , deck = model.deck
                , values = model.game.values
                }
                list

        Just EndOfRound ->
            View.Overlay.gameEnd
                { yourTurn = model.yourTurn
                , onNextRound =
                    EndRoundAndOpenShop
                        currentScore
                }
                model.game

        Just EndOfGame ->
            [ Layout.divWrapper [ Html.Style.displayFlex ] Layout.none
            , [ "You don't have any credits left"
                    |> Layout.divText [ Html.Style.displayFlex, Html.Style.justifyContentCenter ]
              , "Thanks for playing"
                    |> Layout.divText [ Html.Style.displayFlex, Html.Style.fontSizePx 36 ]
              , "Please subscribe and rate the game"
                    |> Layout.divText [ Html.Style.justifyContentCenter ]
              , "Thanks ;)"
                    |> Layout.divText [ Html.Style.justifyContentCenter ]
              ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.gap "16px"
                    , Html.Style.justifyContentSpaceBetween
                    , Html.Style.justifyContentCenter
                    ]
            , View.Ui.button []
                { label = "Restart the game"
                , onPress = Just (Restart model.seed)
                }
            ]
                |> Html.div
                    [ Html.Style.displayFlex
                    , Html.Style.flexDirectionColumn
                    , Html.Style.justifyContentSpaceBetween
                    ]

        Just (Tutorial int) ->
            View.Overlay.tutorial
                { page = int
                , onNext = RequestPageOfTurtorial
                }

        Nothing ->
            model.game
                |> View.Game.toHtml
                    { onChallenge = ChallengeGoal
                    , onPlay = PlayCard
                    , yourTurn = model.yourTurn
                    }
      )
        |> Layout.divWrapper
            [ Html.Style.displayFlex
            , Html.Style.padding "16px"
            , Html.Style.backgroundColor "#dfeaff"
            , Html.Style.width "100%"
            , Html.Style.height "100%"
            , Html.Style.boxSizingBorderBox
            , Html.Style.justifyContentCenter
            ]
    , "CREDITS: "
        ++ String.fromInt model.score
        ++ (if model.overlay == Just EndOfRound then
                (if xor (Game.isWon model.game) model.yourTurn then
                    "+"

                 else
                    ""
                )
                    ++ String.fromInt currentScore

            else
                ""
           )
        |> Layout.divText
            [ Html.Style.displayFlex
            , Html.Style.backgroundColor "#679aff"
            , Html.Style.color "white"
            , Html.Style.padding "32px"
            , Html.Style.boxSizingBorderBox
            , Html.Style.width "100%"
            ]
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=400, initial-scale=1.0"
        ]
        []
    ]
        |> Html.div
            [ Html.Style.displayFlex
            , Html.Style.flexDirectionColumn
            , Html.Style.widthPx 400
            , Html.Style.heightPx 700
            , Html.Style.justifyContentCenter
            , Html.Style.alignItemsCenter
            ]


challengeGoal : Model -> Model
challengeGoal model =
    { model
        | overlay = Just EndOfRound
    }


playCard : CardId -> Model -> Model
playCard id model =
    model.game
        |> Game.Update.playCard id
        |> (\game ->
                { model
                    | game = game
                    , yourTurn = False
                }
           )


requestOpponentTurn : Model -> Model
requestOpponentTurn model =
    Random.step (Game.Update.opponentsTurn model.game)
        model.seed
        |> (\( maybe, seed ) ->
                maybe
                    |> Maybe.map (\game -> { model | yourTurn = True, game = game, seed = seed })
                    |> Maybe.withDefault { model | overlay = Just EndOfRound, seed = seed }
           )


addCardAndStartNextRound : Card -> Model -> Model
addCardAndStartNextRound card model =
    let
        deck =
            card :: model.deck

        multiplier =
            model.multiplier * 1.1

        ( game, newSeed ) =
            Game.fromDeck multiplier deck
                |> (\rand ->
                        Random.step rand model.seed
                   )
    in
    { model
        | game = game
        , deck = deck
        , seed = newSeed
        , overlay = Nothing
        , specialCards = model.specialCards |> List.filter ((/=) card)
        , multiplier = multiplier
    }


endRoundAndOpenShop : Int -> Model -> Model
endRoundAndOpenShop score model =
    let
        newScore =
            model.score + score
    in
    if newScore > 0 then
        { model
            | score = newScore
            , overlay =
                model.specialCards
                    |> List.take 2
                    |> Shop
                    |> Just
            , yourTurn = False
            , specialCards =
                (model.specialCards |> List.drop 2)
                    ++ (model.specialCards |> List.take 2)
        }

    else
        { model | score = 0, overlay = Just EndOfGame }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart seed ->
            ( restartGame seed
            , Process.sleep 1000
                |> Task.perform (\() -> RequestOpponentTurn)
            )

        PlayCard card ->
            ( playCard card model
            , Process.sleep 1000
                |> Task.perform (\() -> RequestOpponentTurn)
            )

        ChallengeGoal ->
            ( challengeGoal model
            , Cmd.none
            )

        RequestOpponentTurn ->
            ( requestOpponentTurn model, Cmd.none )

        EndRoundAndOpenShop score ->
            ( endRoundAndOpenShop score model
            , Cmd.none
            )

        AddCardAndStartNextRound card ->
            ( addCardAndStartNextRound card model
            , if model.score > 0 then
                Process.sleep 1000
                    |> Task.perform (\() -> RequestOpponentTurn)

              else
                Cmd.none
            )

        RequestPageOfTurtorial n ->
            ( { model
                | overlay =
                    if n == 4 then
                        Nothing

                    else
                        Just (Tutorial n)
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
