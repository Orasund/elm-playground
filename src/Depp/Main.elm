module Depp.Main exposing (main)

import Browser exposing (Document)
import Depp.Data.Game as Game exposing (Action, Game)
import Depp.View as View
import Depp.View.Action as Action
import Depp.View.Card as Card
import Random exposing (Seed)
import Set.Any as AnySet exposing (AnySet)


type alias Model =
    { game : Game
    , seed : Seed
    }


type Msg
    = Restart Seed
    | PlayAction Action


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


newModel : Seed -> Model
newModel old =
    old
        |> Random.step Game.new
        |> (\( game, seed ) -> { game = game, seed = seed })


init : () -> ( Model, Cmd Msg )
init () =
    ( Random.initialSeed 42 |> newModel
    , Random.generate Restart Random.independentSeed
    )


view : Model -> Document Msg
view model =
    let
        actions =
            model.game
                |> Game.actions
                |> List.sortBy
                    (\action ->
                        case action of
                            Game.PlayCard args ->
                                Game.value args.hand

                            Game.Redraw _ ->
                                0
                    )
    in
    { title = "Depp Card Game"
    , body =
        [ View.stylesheet
        , model.game.hand
            |> AnySet.toList
            |> List.map Card.view
            |> View.collection "Hand"
        , model.game.board
            |> AnySet.toList
            |> List.map
                (\card ->
                    card
                        |> Card.withActions
                            (actions
                                |> List.filterMap
                                    (\action ->
                                        case action of
                                            Game.PlayCard args ->
                                                if args.board == card then
                                                    { label = "Play " ++ Card.toString args.hand
                                                    , onClick = Just (PlayAction action)
                                                    }
                                                        |> Just

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                    )
                                |> List.take 1
                            )
                )
            |> View.collection ("Board (" ++ (model.game.drawPile |> List.length |> String.fromInt) ++ " remaining)")
        , actions
            |> List.map (Action.view PlayAction)
            |> (::) { label = "New Game", onClick = Just (Restart model.seed) }
            |> View.actionGroup "List of all possible Actions"
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart seed ->
            ( newModel seed, Cmd.none )

        PlayAction action ->
            Random.step (Game.play action model.game) model.seed
                |> (\( game, seed ) ->
                        ( { model | game = game, seed = seed }, Cmd.none )
                   )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
