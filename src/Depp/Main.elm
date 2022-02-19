module Depp.Main exposing (main)

import Browser exposing (Document)
import Depp.Data.Deck as Deck exposing (Card)
import Depp.Data.Game as Game exposing (Action, Game)
import Depp.Data.Rule as Rule
import Depp.Layout as Layout
import Depp.View as View exposing (card)
import Depp.View.Action as Action
import Depp.View.Card as Card
import Dict.Any as AnyDict
import Html
import Html.Attributes as Attr
import List.Extra as List
import Maybe.Extra as Maybe
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
    | ToggleBoardCard Card
    | ToggleHandCard Card


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
        , [ model.game.hand
                |> AnySet.toList
                |> List.map
                    (\card ->
                        ( Just card == model.hand
                        , { label = card |> Card.toString model.game
                          , onClick = Just (ToggleHandCard card)
                          }
                        )
                    )
                |> View.actionSelect "Hand"
          , model.game.board
                |> AnySet.toList
                |> List.map
                    (\card ->
                        ( Just card == model.board
                        , { label = card |> Card.toString model.game
                          , onClick = Just (ToggleBoardCard card)
                          }
                        )
                    )
                |> View.actionSelect "Board"
          , model.game.drawPile
                |> List.map .suit
                |> List.gatherEquals
                |> List.map
                    (\( s, l ) ->
                        [ (l
                            |> List.length
                            |> (+) 1
                            |> String.fromInt
                          )
                            ++ "x"
                            |> Html.text
                        , Game.suitToString s model.game
                            |> Html.text
                        ]
                            |> Layout.chip
                    )
                |> View.collection "Remaining"
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
          ]
            |> Html.article []
            |> List.singleton
            |> Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "padding" "16px"
                ]
        , actions
            |> List.filterMap
                (\action ->
                    case action of
                        Game.PlayCard args ->
                            if
                                (model.board |> Maybe.map ((==) args.board) |> Maybe.withDefault True)
                                    && (model.hand |> Maybe.map ((==) args.hand) |> Maybe.withDefault True)
                            then
                                action
                                    |> Action.view model.game PlayAction
                                    |> Just

                            else
                                Nothing

                        Game.SwapCards args ->
                            if
                                (model.board |> Maybe.map ((==) args.board) |> Maybe.withDefault True)
                                    && (model.hand |> Maybe.map ((==) args.hand) |> Maybe.withDefault True)
                            then
                                action
                                    |> Action.view model.game PlayAction
                                    |> Just

                            else
                                Nothing

                        Game.Redraw hand ->
                            if model.hand |> Maybe.map ((==) hand) |> Maybe.withDefault True then
                                action
                                    |> Action.view model.game PlayAction
                                    |> Just

                            else
                                Nothing
                )
            |> (::) { label = "New Game", onClick = Just (Restart model.seed) }
            |> View.actionGroup "Actions"
            |> Layout.footer
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

        ToggleBoardCard card ->
            ( { model
                | board =
                    if model.board == Just card then
                        Nothing

                    else
                        Just card
              }
            , Cmd.none
            )

        ToggleHandCard card ->
            ( { model
                | hand =
                    if model.hand == Just card then
                        Nothing

                    else
                        Just card
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
