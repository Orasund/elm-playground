module Ecocards.Page.Playground exposing (Model)

import Dict
import Ecocards.Data.Animal exposing (Behaviour(..))
import Ecocards.Data.Game as Game exposing (Game)
import Ecocards.Data.GamePhase as GamePhase exposing (GamePhase(..))
import Ecocards.Data.Move as Move
import Set


type alias Model =
    { game : Game
    , phase : GamePhase
    }


type Msg
    = ClickedCard Int
    | Confirmed


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedCard id ->
            model.game.animals
                |> Dict.get id
                |> Maybe.map
                    (\animal ->
                        case model.phase of
                            WaitingForOpponent ->
                                model

                            Thinking { played } ->
                                { model
                                    | phase =
                                        let
                                            ( minAnimal, maxAnimal ) =
                                                case animal.behaviour of
                                                    Predator _ amounts ->
                                                        amounts

                                                    Herbivores _ ->
                                                        ( 0, 0 )

                                                    Omnivorous amounts ->
                                                        amounts
                                        in
                                        Tapping
                                            { card = id
                                            , selected = Set.empty
                                            , played = played
                                            , maxAmount = minAnimal
                                            , minAmount = maxAnimal
                                            }
                                }

                            Tapping move ->
                                { model
                                    | phase = Tapping (move |> Move.toggle id)
                                }

                            Finished _ ->
                                model
                    )
                |> Maybe.withDefault model

        Confirmed ->
            GamePhase.end ( model.phase, model.game )
                |> (\( phase, game ) -> { phase = phase, game = game })
