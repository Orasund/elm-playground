module Main exposing (..)

import Browser
import Game exposing (Card, Game)
import Game.Update
import Goal
import Html exposing (Html)
import Html.Attributes
import Html.Style as Style
import Layout
import Process
import Random exposing (Seed)
import Task
import View.Game
import View.Overlay
import View.Ui


type Overlay
    = EndOfRound
    | EndOfGame


type alias Model =
    { game : Game
    , seed : Seed
    , overlay : Maybe Overlay
    , yourTurn : Bool
    , score : Int
    }


type Msg
    = Restart Seed
    | PlayCard Card
    | ChallengeGoal
    | RequestOpponentTurn
    | NewRoundRequested Int


init : () -> ( Model, Cmd Msg )
init () =
    ( restartGame (Random.initialSeed 42)
    , Random.independentSeed
        |> Random.generate Restart
    )


restartGame : Seed -> Model
restartGame seed =
    let
        ( game, newSeed ) =
            Game.fromGoals Goal.asList
                |> (\rand ->
                        Random.step rand seed
                   )
    in
    { game = game
    , seed = newSeed
    , overlay = Nothing
    , yourTurn = False
    , score = 100
    }


view : Model -> Html Msg
view model =
    let
        currentScore =
            model.game.playedCards
                |> List.head
                |> Maybe.map (\card -> Goal.probability card.goal)
                |> Maybe.withDefault 0
                |> (*)
                    (if xor (Game.isWon model.game) model.yourTurn then
                        1

                     else
                        -2
                    )
    in
    [ (case model.overlay of
        Just EndOfRound ->
            View.Overlay.gameEnd
                { yourTurn = model.yourTurn
                , onNextRound =
                    NewRoundRequested
                        currentScore
                }
                model.game

        Just EndOfGame ->
            [ Layout.el [] Layout.none
            , [ "You don't have any credits left"
                    |> Layout.text [ Style.justifyContentCenter ]
              , "Thanks for playing"
                    |> Layout.text [ Html.Attributes.style "font-size" "36px" ]
              , "Please subscribe and rate the game"
                    |> Layout.text [ Style.justifyContentCenter ]
              , "Thanks ;)"
                    |> Layout.text [ Style.justifyContentCenter ]
              ]
                |> Layout.column
                    [ Style.gap "16px"
                    , Layout.contentWithSpaceBetween
                    , Style.justifyContentCenter
                    ]
            , View.Ui.button []
                { label = "Restart the game"
                , onPress = Just (Restart model.seed)
                }
            ]
                |> Layout.column [ Layout.contentWithSpaceBetween ]

        Nothing ->
            model.game
                |> View.Game.toHtml
                    { onChallenge = ChallengeGoal
                    , onPlay = PlayCard
                    , yourTurn = model.yourTurn
                    }
      )
        |> Layout.el
            [ Html.Attributes.style "padding" "16px"
            , Html.Attributes.style "background-color" "#dfeaff"
            , Style.width "100%"
            , Style.height "100%"
            , Style.boxSizingBorderBox
            , Style.justifyContentCenter
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
        |> Layout.text
            [ Html.Attributes.style "background-color" "#679aff"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "padding" "32px"
            , Style.boxSizingBorderBox
            , Style.width "100%"
            ]
    , Html.node "meta"
        [ Html.Attributes.name "viewport"
        , Html.Attributes.attribute "content" "width=400, initial-scale=1.0"
        ]
        []
    ]
        |> Layout.column
            ([ Style.width "400px"
             , Style.height "700px"
             ]
                ++ Layout.centered
            )


challengeGoal : Model -> Model
challengeGoal model =
    { model
        | overlay = Just EndOfRound
    }


playCard : Card -> Model -> Model
playCard card model =
    model.game
        |> Game.Update.playCard card
        |> (\game ->
                { model
                    | game = game
                    , yourTurn = False
                }
           )


requestOpponentTurn : Model -> Model
requestOpponentTurn model =
    model.game
        |> Game.Update.opponentsTurn
        |> Maybe.map (\game -> { model | yourTurn = True, game = game })
        |> Maybe.withDefault { model | overlay = Just EndOfRound }


newGameRequested : Int -> Model -> Model
newGameRequested score model =
    let
        ( game, newSeed ) =
            Game.fromGoals Goal.asList
                |> (\rand ->
                        Random.step rand model.seed
                   )

        newScore =
            model.score + score
    in
    if newScore > 0 then
        { model
            | game = game
            , seed = newSeed
            , overlay = Nothing
            , yourTurn = False
            , score = newScore
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

        NewRoundRequested score ->
            ( newGameRequested score model
            , if model.score + score > 0 then
                Process.sleep 1000
                    |> Task.perform (\() -> RequestOpponentTurn)

              else
                Cmd.none
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
