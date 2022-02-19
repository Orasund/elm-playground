module Depp.Main exposing (main)

import Browser exposing (Document)
import Depp.Data.Deck as Deck exposing (Card)
import Depp.Data.Game as Game exposing (Action, Game)
import Depp.Data.Rule as Rule
import Depp.View as View
import Depp.View.Action as Action
import Depp.View.Card as Card
import Dict.Any as AnyDict
import Html
import List.Extra as List
import Random exposing (Seed)
import Set.Any as AnySet exposing (AnySet)


type alias Model =
    { game : Game
    , seed : Seed
    , hand : Maybe Card
    , board : Maybe Card
    }


type Msg
    = Restart Seed
    | PlayAction Action
    | SelectBoardCard Card


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
        |> (\( game, seed ) ->
                { game = game
                , seed = seed
                , hand = Nothing
                , board = Nothing
                }
           )


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
                                Game.value model.game args.hand

                            Game.SwapCards args ->
                                Game.value model.game args.hand

                            Game.Redraw card ->
                                Game.value model.game card
                    )
    in
    { title = "Depp Card Game"
    , body =
        [ View.stylesheet
        , model.game.hand
            |> AnySet.toList
            |> List.map (Card.view model.game)
            |> View.collection "Hand"
        , model.game.drawPile
            |> List.map .suit
            |> List.gatherEquals
            |> List.map
                (\( s, l ) ->
                    (l
                        |> List.length
                        |> (+) 1
                        |> String.fromInt
                    )
                        ++ "x"
                        ++ Game.suitToString s model.game
                        |> Html.text
                        |> List.singleton
                        |> Html.div []
                )
            |> View.collection "Remaining"
        , model.game.board
            |> AnySet.toList
            |> List.map
                (\card ->
                    { label = card |> Card.toString model.game
                    , onClick = Just (SelectBoardCard card)
                    }
                )
            |> View.actionGroup "Board"
        , model.board
            |> Maybe.map
                (\card ->
                    actions
                        |> List.filterMap
                            (\action ->
                                case action of
                                    Game.PlayCard args ->
                                        if args.board == card then
                                            { label = "Play " ++ Card.toString model.game args.hand
                                            , onClick = Just (PlayAction action)
                                            }
                                                |> Just

                                        else
                                            Nothing

                                    Game.SwapCards args ->
                                        if args.board == card then
                                            { label = "Swap " ++ Card.toString model.game args.hand
                                            , onClick = Just (PlayAction action)
                                            }
                                                |> Just

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> View.actionGroup ("Actions for " ++ Card.toString model.game card)
                )
            |> Maybe.withDefault (Html.text "Select a card")
        , model.game.rules
            |> AnyDict.toList
            |> List.map
                (\( rule, suit ) ->
                    Game.suitToString suit model.game
                        ++ " "
                        ++ Rule.toString rule
                        |> Html.text
                )
            |> View.listing "Rules"
        , actions
            |> List.map (Action.view model.game PlayAction)
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
                        ( { model
                            | game = game
                            , seed = seed
                            , board = Nothing
                            , hand = Nothing
                          }
                        , Cmd.none
                        )
                   )

        SelectBoardCard card ->
            ( { model | board = Just card }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
