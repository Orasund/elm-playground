module Depp.Main exposing (main)

import Array
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

        drawPile =
            model.game.drawPile
                |> List.map .suit
                |> List.gatherEquals
                |> AnyDict.fromList Deck.suitToInt

        ruleBySuit =
            model.game.rules
                |> AnyDict.toList
                |> List.gatherEqualsBy Tuple.second
                |> List.map
                    (\( first, rest ) ->
                        ( Tuple.second first, first :: rest )
                    )
                |> AnyDict.fromList Deck.suitToInt
    in
    { title = "Depp Card Game"
    , body =
        [ View.stylesheet
        , (((Html.text "Suit"
                |> List.singleton
                |> Html.div [ Attr.style "text-align" "center", Attr.style "width" "100px" ]
            )
                :: ([ "Hand", "Board" ]
                        |> List.map
                            (\string ->
                                string
                                    |> Html.text
                                    |> List.singleton
                                    |> Html.div [ Attr.style "flex" "1" ]
                            )
                   )
                |> Layout.row
                    ([ Attr.style "background-color" "#fff"
                     , Attr.style "border-bottom" "rgba(0,0,0,0.1) solid 1px"
                     , Attr.style "padding" "8px 0"
                     ]
                        ++ Layout.stickyOnTop
                    )
           )
            :: (Deck.suits
                    |> Array.toList
                    |> List.map
                        (\suit ->
                            let
                                rules =
                                    ruleBySuit
                                        |> AnyDict.get suit
                                        |> Maybe.withDefault []
                            in
                            (([ Game.suitToString suit model.game
                                    |> Html.text
                                    |> List.singleton
                                    |> Layout.chip
                                        [ Attr.style "width" "32px"
                                        , Attr.style "height" "32px"
                                        ]
                              , drawPile
                                    |> AnyDict.get suit
                                    |> Maybe.map
                                        (\list ->
                                            (list |> List.length |> (+) 1 |> String.fromInt) ++ " remain"
                                        )
                                    |> Maybe.withDefault ""
                                    |> Html.text
                              ]
                                |> Layout.column
                                    [ Layout.gap 4
                                    , Layout.alignCenter
                                    ]
                                |> List.singleton
                                |> Html.div
                                    [ Attr.style "width" "100px"
                                    ]
                             )
                                :: ([ model.game.hand
                                        |> AnySet.toList
                                        |> List.filter (\card -> card.suit == suit)
                                        |> List.map
                                            (\card ->
                                                View.selectButton
                                                    ( Just card == model.hand
                                                    , { label = card |> Card.toString model.game
                                                      , onClick = Just (ToggleHandCard card)
                                                      }
                                                    )
                                            )
                                        |> Layout.row [ Layout.gap 8 ]
                                    , model.game.board
                                        |> AnySet.toList
                                        |> List.filter (\card -> card.suit == suit)
                                        |> List.map
                                            (\card ->
                                                View.selectButton
                                                    ( Just card == model.board
                                                    , { label = card |> Card.toString model.game
                                                      , onClick = Just (ToggleBoardCard card)
                                                      }
                                                    )
                                            )
                                        |> Layout.row [ Layout.gap 8 ]
                                    ]
                                        |> List.map
                                            (\elem ->
                                                elem
                                                    |> List.singleton
                                                    |> Html.div [ Attr.style "flex" "1" ]
                                            )
                                   )
                            )
                                :: (rules
                                        |> List.map
                                            (\( rule, _ ) ->
                                                [ Html.div [ Attr.style "width" "100px" ] []
                                                , Game.suitToString suit model.game
                                                    ++ " "
                                                    ++ Rule.toString rule
                                                    |> Html.text
                                                    |> List.singleton
                                                    |> Html.div [ Attr.style "flex" "2" ]
                                                ]
                                            )
                                   )
                                |> List.map
                                    (Layout.row
                                        [ Attr.style "width" "100%"
                                        , Layout.gap 4
                                        , Layout.alignBaseline
                                        ]
                                    )
                        )
                    |> List.map
                        (Layout.row
                            [ Attr.style "width" "100%"
                            , Attr.style "border-bottom" "rgba(0,0,0,0.1) solid 1px"
                            , Attr.style "padding" "8px 0"
                            ]
                        )
               )
          )
            |> Layout.column []
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
                            if model.hand |> Maybe.map ((==) hand) |> Maybe.withDefault False then
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
