module Main exposing (main)

import Action exposing (Action(..))
import Artefact exposing (Artefact(..))
import Browser
import Dict exposing (Dict)
import Game.Generate
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import Overlay exposing (Overlay(..))
import Piece exposing (Piece(..))
import Process
import Random exposing (Seed)
import Task
import View.Artefact
import View.Level
import View.Overlay
import View.Shop


type alias Model =
    { level : Level
    , artefacts : Dict String Artefact
    , selected : Maybe ( Int, Int )
    , levelCount : Int
    , overlay : Maybe Overlay
    , seed : Seed
    , movementOverride : Maybe Piece
    }


type Msg
    = Select (Maybe ( Int, Int ))
    | RequestOpponentMove
    | GotSeed Seed
    | EndLevel
    | Activate Artefact
    | CloseOverlay Action


init : () -> ( Model, Cmd Msg )
init () =
    let
        lv =
            1

        party =
            [ King ]

        ( level, seed ) =
            Random.step
                (Game.Generate.generateByLevel lv
                    party
                )
                (Random.initialSeed 42)
    in
    ( { level = level
      , artefacts = Dict.empty
      , selected = Nothing
      , levelCount = lv
      , overlay = Nothing
      , seed = seed
      , movementOverride = Nothing
      }
    , Random.generate GotSeed Random.independentSeed
    )


startLevel : List Piece -> Model -> ( Model, Cmd Msg )
startLevel party model =
    let
        levelCount =
            model.levelCount + 1

        ( level, seed ) =
            Random.step
                (Game.Generate.generateByLevel levelCount
                    party
                )
                model.seed
    in
    ( { model
        | level = level
        , seed = seed
        , levelCount = levelCount
        , selected = Nothing
        , overlay = Nothing
        , movementOverride = Nothing
      }
    , Cmd.none
    )


applyAction : Action -> Model -> ( Model, Cmd Msg )
applyAction action model =
    case action of
        ResetLevel ->
            startLevel
                (model.level.board
                    |> Dict.filter (\_ square -> square.isWhite)
                    |> Dict.toList
                    |> List.map (\( _, { piece } ) -> piece)
                )
                { model
                    | levelCount = model.levelCount - 1
                }

        NextLevel party ->
            startLevel party model

        RemoveArtefactAnd artefact action2 ->
            { model | artefacts = model.artefacts |> Dict.remove (Artefact.name artefact) }
                |> applyAction action2

        AddArtefactAnd artefact action2 ->
            { model | artefacts = model.artefacts |> Dict.insert (Artefact.name artefact) artefact }
                |> applyAction action2

        UndoMove ->
            ( { model | level = model.level |> Level.undo }
            , Cmd.none
            )

        FindArtefact ->
            ( Random.step
                (case
                    Artefact.list
                        |> List.filter
                            (\artefact ->
                                model.artefacts
                                    |> Dict.values
                                    |> List.member artefact
                                    |> not
                            )
                 of
                    head :: tail ->
                        Random.uniform head tail

                    [] ->
                        Random.constant EscapeRope
                )
                model.seed
                |> (\( artefact, seed ) ->
                        { model
                            | overlay = Just (FoundArtefactOverlay artefact)
                            , level = model.level |> (\l -> { l | loot = Nothing })
                            , seed = seed
                        }
                   )
            , Cmd.none
            )

        EndMove ->
            ( { model | movementOverride = Nothing }
            , Process.sleep 100
                |> Task.perform (\() -> RequestOpponentMove)
            )

        OverrideMovement piece ->
            ( { model | movementOverride = Just piece }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select maybe ->
            case model.selected of
                Nothing ->
                    ( { model | selected = maybe }, Cmd.none )

                Just selected ->
                    case maybe of
                        Nothing ->
                            ( { model | selected = maybe }, Cmd.none )

                        Just to ->
                            model.level
                                |> Level.move { from = selected, to = to }
                                |> (\level ->
                                        { model
                                            | level = level
                                            , selected = Nothing
                                        }
                                   )
                                |> applyAction
                                    (if model.level.loot == Just to then
                                        FindArtefact

                                     else
                                        EndMove
                                    )

        RequestOpponentMove ->
            (case model.level |> Level.findNextMove of
                Just args ->
                    { model
                        | level =
                            Level.move args model.level
                    }

                Nothing ->
                    case model.level |> Level.possibleMoves { isYourTurn = True } of
                        head :: tail ->
                            Random.step (Random.uniform head tail)
                                model.seed
                                |> (\( move, seed ) ->
                                        { model
                                            | level =
                                                model.level
                                                    |> Level.move move
                                            , seed = seed
                                        }
                                   )

                        [] ->
                            model
            )
                |> (\m -> ( m, Cmd.none ))

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        EndLevel ->
            ( model.level.board
                |> Dict.filter (\_ square -> square.isWhite)
                |> Dict.toList
                |> List.map (\( _, { piece } ) -> piece)
                |> (\party ->
                        { model
                            | overlay = ShopOverlay { party = party } |> Just
                        }
                   )
            , Cmd.none
            )

        Activate artefact ->
            applyAction (Action.fromArtefact artefact)
                { model
                    | artefacts = model.artefacts |> Dict.remove (Artefact.name artefact)
                }

        CloseOverlay action ->
            applyAction action
                { model | overlay = Nothing }


view : Model -> Html Msg
view model =
    (case model.overlay of
        Just (ShopOverlay { party }) ->
            View.Shop.toHtml
                { party = party
                , onCloseOverlay = CloseOverlay
                }

        Just (FoundArtefactOverlay artefact) ->
            View.Overlay.foundArtefact
                { onCloseOverlay = CloseOverlay
                , artefacts = model.artefacts |> Dict.values
                }
                artefact

        Nothing ->
            [ View.Level.toHtml
                { selected = model.selected
                , onSelect = Select
                , movementOverride = model.movementOverride
                }
                model.level
            , if Level.isWon model.level then
                Layout.textButton []
                    { label = "Next Level"
                    , onPress = Just EndLevel
                    }

              else if Level.isLost model.level then
                Layout.text [] "You Lost"

              else
                model.artefacts
                    |> Dict.values
                    |> View.Artefact.toHtml
                        { onActivate = Activate }
            ]
                |> Layout.column [ Layout.gap 8 ]
    )
        |> Layout.el [ Html.Attributes.style "width" "200px" ]


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
