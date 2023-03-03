module Main exposing (..)

import Action exposing (Action)
import Browser exposing (Document)
import Card exposing (CardId)
import Config
import Dict
import Game exposing (Game)
import Game.Area
import Game.Card
import Game.Entity
import Html
import Html.Attributes
import Html.Events
import Layout
import Random exposing (Generator, Seed)
import Time
import View


type alias Model =
    { actions : List Action
    , game : Game
    , selected : Maybe CardId
    , seed : Seed
    }


type Msg
    = NextAction
    | SelectedCard (Maybe CardId)
    | GotSeed Seed


init : () -> ( Model, Cmd Msg )
init () =
    ( { actions = [ Action.Shuffle, Action.DrawCards 7 ]
      , game = Game.init
      , selected = Nothing
      , seed = Random.initialSeed 42
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    let
        cardHeight =
            200

        cardWidth =
            cardHeight * 2 / 3

        spacing =
            32

        padding =
            32

        selectedCard =
            model.selected
                |> Maybe.andThen (Game.getCardFrom model.game)
    in
    { title = "Enough For Now"
    , body =
        [ [ model.game.hand
                |> Dict.values
                |> List.concat
                |> List.filter (\cardId -> Just cardId /= model.selected)
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i ( cardId, card ) ->
                        ( cardId, card )
                            |> View.card [ Html.Events.onClick (SelectedCard (Just cardId)) ]
                                True
                            |> Game.Entity.move ( toFloat i * (cardWidth * 2 + spacing * 2) / (Config.cardsInHand - 1 |> toFloat), 0 )
                            |> Game.Entity.mapZIndex
                                (if
                                    selectedCard
                                        |> Maybe.map (\( _, c ) -> Card.cardType c == Card.cardType card)
                                        |> Maybe.withDefault False
                                 then
                                    (+) 150

                                 else
                                    (+) 50
                                )
                    )
                |> Game.Area.pileAbove ( padding, cardHeight * 2 + spacing * 2 + padding )
                    ( "hand", \attrs -> [ Html.text "" ] |> Html.div attrs )
          , model.game.drawPile
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i ( cardId, card ) ->
                        View.card [] False ( cardId, card )
                            |> Game.Entity.move ( 0, toFloat i * -4 )
                    )
                |> Game.Area.pileAbove ( padding, padding )
                    ( "Draw Pile", \attrs -> Game.Card.empty attrs "Draw Pile" )
          , model.game.discardPile
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i tuple ->
                        View.card [] True tuple
                            |> Game.Entity.move ( 0, toFloat i * -4 )
                    )
                |> Game.Area.pileAbove ( cardWidth * 2 + spacing * 2 + padding, padding )
                    ( "Discard Pile", \attrs -> Game.Card.empty attrs "Discard Pile" )
          , model.game.graveyard
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i tuple ->
                        View.card [] True tuple
                            |> Game.Entity.move ( 0, toFloat i * -4 )
                    )
                |> Game.Area.pileAbove ( cardWidth + spacing + padding, cardHeight + spacing + padding )
                    ( "Graveyard", \attrs -> Layout.none |> Layout.el attrs )
          , ( "overlay"
            , \attrs ->
                Layout.none
                    |> Layout.el
                        ((if model.selected /= Nothing then
                            [ Html.Attributes.style "backdrop-filter" "blur(4px)"
                            ]

                          else
                            [ Html.Attributes.style "backdrop-filter" "blur(0px)"
                            ]
                         )
                            ++ [ Html.Attributes.style "height" "100%"
                               , Html.Attributes.style "width" "100%"
                               , Html.Attributes.style "transition" "backdrop-filter 2s"
                               , Html.Events.onClick (SelectedCard Nothing)
                               ]
                        )
                    |> Layout.el
                        ([ Html.Attributes.style "height" "100%"
                         , Html.Attributes.style "width" "100%"
                         ]
                            ++ attrs
                        )
            )
                |> Game.Entity.new
                |> Game.Entity.mapZIndex
                    (if model.selected /= Nothing then
                        (+) 100

                     else
                        \_ -> 0
                    )
                |> List.singleton
          , selectedCard
                |> Maybe.map
                    (\tuple ->
                        View.card [] True tuple
                            |> Game.Entity.move ( cardWidth + spacing + padding, cardHeight / 2 + padding )
                            |> Game.Entity.mapZIndex ((+) 101)
                            |> Game.Entity.mapCustomTransformations ((++) [ Game.Entity.scale 2 ])
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
          ]
            |> List.concat
            |> Game.Area.toHtml
                [ Html.Attributes.style "height" (String.fromInt (cardHeight * 3 + spacing * 2 + padding * 2) ++ "px")
                , Html.Attributes.style "width" (String.fromFloat (cardWidth * 3 + spacing * 2 + padding * 2) ++ "px")
                ]
            |> Layout.el (Html.Attributes.style "height" "100%" :: Layout.centered)
        , Html.node "style" [] [ Html.text ":root,body {height:100%;background-color:#f4f3ee}" ]
        ]
    }


applyGeneratorTo : Model -> Generator Model -> Model
applyGeneratorTo model gen =
    let
        ( m, seed ) =
            Random.step gen model.seed
    in
    { m | seed = seed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextAction ->
            case model.actions of
                head :: tail ->
                    ( model.game
                        |> Game.applyAction head
                        |> Random.map
                            (\( game, actions ) ->
                                { model | game = game, actions = actions ++ tail }
                            )
                        |> applyGeneratorTo model
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        SelectedCard maybeCardId ->
            case
                model.selected
                    |> Maybe.andThen (Game.getCardFrom model.game)
                    |> Maybe.map2 Tuple.pair maybeCardId
            of
                Just ( cardId2, ( cardId1, card ) ) ->
                    ( { model
                        | actions =
                            [ Action.MoveToArea [ cardId1, cardId2 ]
                            , Action.ClearArea
                            ]
                                ++ Action.fromCard card
                                ++ model.actions
                        , selected = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | selected = maybeCardId }, Cmd.none )

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 (\_ -> NextAction)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
