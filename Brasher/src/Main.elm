port module Main exposing (..)

import Action exposing (Action)
import Browser exposing (Document)
import Browser.Events
import Config
import Dict
import Game exposing (Game)
import Html
import Html.Attributes
import Html.Keyed
import Json.Decode
import Layout
import Random exposing (Generator, Seed)
import Sound
import Tile exposing (Tile)
import Time
import View


port loadSound : ( String, String ) -> Cmd msg


port playSound : String -> Cmd msg


port setVolume : Float -> Cmd msg


type Overlay
    = GameOver { killedBy : Tile }
    | StageCleared


type alias Model =
    { game : Game
    , actions : List Action
    , level : Int
    , score : Int
    , maxScore : Int
    , seed : Seed
    , overlay : Maybe Overlay
    }


type Msg
    = Move { left : Bool }
    | GotSeed Seed
    | NextAction
    | CloseOverlay


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.init
      , actions = []
      , level = 0
      , score = 0
      , maxScore = 0
      , seed = Random.initialSeed 42
      , overlay = Nothing
      }
    , Random.generate GotSeed Random.independentSeed
    )


view : Model -> Document Msg
view model =
    { title = "Brasher"
    , body =
        [ [ Layout.el [ Layout.fill ] Layout.none
          , model.score
                |> String.fromInt
                |> Layout.text
                    [ Html.Attributes.style "font-size" "120px"
                    ]
          , "Highscore "
                ++ String.fromInt model.maxScore
                |> Layout.text
                    [ Html.Attributes.style "font-size" "16px"
                    , Layout.fill
                    ]
          ]
            |> Layout.row
                [ Layout.contentWithSpaceBetween
                , Html.Attributes.style "width" "100%"
                , Layout.alignAtBaseline
                ]
        , model.game.world
            |> Dict.toList
            |> List.filterMap
                (\( i, tileId ) ->
                    model.game.tiles
                        |> Dict.get tileId
                        |> Maybe.map (\tile -> ( i, ( tileId, tile ) ))
                )
            |> List.map
                (\( i, ( tileId, tile ) ) ->
                    ( String.fromInt tileId
                    , tile
                        |> Tile.emoji
                        |> Layout.text
                            [ Html.Attributes.style "position" "absolute"
                            , Html.Attributes.style "left"
                                (String.fromInt ((Config.maxDistance + i) * 30) ++ "px")
                            , Html.Attributes.style "font-size" "32px"
                            , Html.Attributes.style "transition" "left 0.1s"
                            ]
                    )
                )
            |> (\l ->
                    Html.Keyed.node "div"
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "position" "relative"
                        , Html.Attributes.style "width"
                            ((30 * (Config.maxDistance * 2 + 1) |> String.fromInt) ++ "px")
                        , Html.Attributes.style "height" "40px"
                        ]
                        l
               )
        , [ Game.nextTileLeftOf model.game.playerPos model.game
                |> Maybe.andThen
                    (\( _, tile ) ->
                        if model.actions == [] then
                            [ "Move left and "
                                ++ Tile.description tile
                                |> Layout.text
                                    [ Html.Attributes.style "font-size" "22px"
                                    , Html.Attributes.style "text-align" "center"
                                    ]
                            , Tile.emoji tile |> Layout.text [ Html.Attributes.style "font-size" "90px" ]
                            ]
                                |> View.card
                                    (Layout.asButton
                                        { label = "Move Left"
                                        , onPress = Move { left = True } |> Just
                                        }
                                    )
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.withDefault View.emptyCard
          , "or"
                |> Layout.text []
          , Game.nextTileRightOf model.game.playerPos model.game
                |> Maybe.andThen
                    (\( _, tile ) ->
                        if model.actions == [] then
                            [ "Move right and "
                                ++ Tile.description tile
                                |> Layout.text
                                    [ Html.Attributes.style "font-size" "22px"
                                    , Html.Attributes.style "text-align" "center"
                                    ]
                            , Tile.emoji tile |> Layout.text [ Html.Attributes.style "font-size" "90px" ]
                            ]
                                |> View.card
                                    (Layout.asButton
                                        { label = "Move Right"
                                        , onPress = Move { left = False } |> Just
                                        }
                                    )
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.withDefault View.emptyCard
          ]
            |> Layout.row ([ Html.Attributes.style "width" "100%", Layout.gap 8 ] ++ Layout.centered)
        , "Click a card or use the A and D keys to play the game" |> Layout.text []
        ]
            |> Layout.column
                [ Html.Attributes.style "width" "400px"
                , Html.Attributes.style "height" "600px"
                , Html.Attributes.style "padding" "8px"
                , Layout.alignAtCenter
                , Layout.contentWithSpaceBetween
                ]
            |> Layout.el
                ([ Html.Attributes.style "height" "100%"
                 , Html.Attributes.style "width" "100%"
                 ]
                    ++ Layout.centered
                )
            |> Layout.withStack []
                (case model.overlay of
                    Nothing ->
                        []

                    Just StageCleared ->
                        [ \attrs ->
                            "Level "
                                ++ String.fromInt model.level
                                |> Layout.text
                                    ([ Html.Attributes.style "width" "100%"
                                     , Html.Attributes.style "height" "200px"
                                     , Html.Attributes.style "background-color" "green"
                                     , Html.Attributes.style "color" "white"
                                     , Html.Attributes.style "font-size" "64px"
                                     ]
                                        ++ Layout.centered
                                    )
                                |> Layout.el
                                    ([ Html.Attributes.style "width" "100%"
                                     , Html.Attributes.style "height" "100%"
                                     , Html.Attributes.style "backdrop-filter" "blur(1px)"
                                     , Html.Attributes.style "z-index" "2"
                                     ]
                                        ++ Layout.asButton
                                            { label = "Close Overlay"
                                            , onPress = Just CloseOverlay
                                            }
                                        ++ attrs
                                        ++ Layout.centered
                                    )
                        ]

                    Just (GameOver { killedBy }) ->
                        [ \attrs ->
                            "Game Over "
                                ++ Tile.emoji killedBy
                                |> Layout.text
                                    ([ Html.Attributes.style "width" "100%"
                                     , Html.Attributes.style "height" "200px"
                                     , Html.Attributes.style "background-color" "red"
                                     , Html.Attributes.style "color" "white"
                                     , Html.Attributes.style "font-size" "64px"
                                     ]
                                        ++ Layout.centered
                                    )
                                |> Layout.el
                                    ([ Html.Attributes.style "width" "100%"
                                     , Html.Attributes.style "height" "100%"
                                     , Html.Attributes.style "backdrop-filter" "blur(1px)"
                                     , Html.Attributes.style "z-index" "2"
                                     ]
                                        ++ Layout.asButton
                                            { label = "Close Overlay"
                                            , onPress = Just CloseOverlay
                                            }
                                        ++ attrs
                                        ++ Layout.centered
                                    )
                        ]
                )
            |> (\content ->
                    [ content
                    , [ """
@font-face {
    font-family: "NotoEmojiColor";
    src: url("assets/NotoEmojiColor.ttf");
  }

:root,body {
    height:100%;
    font-family: serif,"NotoEmojiColor";
    margin: 0px
}"""
                            |> Html.text
                      ]
                        |> Html.node "style" []
                    ]
               )
    }


applyAction : Action -> Model -> Generator ( Model, Cmd Msg )
applyAction action model =
    case action of
        Action.Move args ->
            Game.moveTile args model.game
                |> Maybe.map (\game -> ( { model | game = game }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )
                |> Random.constant

        Action.LevelCleared ->
            Game.newLevel (model.level + 1)
                |> Random.map
                    (\game ->
                        ( { model
                            | game = game
                            , level = model.level + 1
                            , actions = []
                            , overlay = Just StageCleared
                          }
                        , Sound.sources
                            |> List.map loadSound
                            |> Cmd.batch
                        )
                    )

        Action.LooseLife args ->
            ( { model
                | game = Game.init
                , score = 0
                , level = 0
                , actions = []
                , overlay = Just (GameOver args)
              }
            , Cmd.none
            )
                |> Random.constant

        Action.Tick ->
            ( { model
                | actions =
                    (Game.tick model.game
                        |> Action.Chain
                    )
                        :: model.actions
              }
            , Cmd.none
            )
                |> Random.constant

        Action.Kill pos ->
            model.game
                |> Game.killTile pos
                |> (\( game, actions ) ->
                        ( { model | game = game, actions = actions ++ model.actions }
                        , Cmd.none
                        )
                   )
                |> Random.constant

        Action.Spawn pos tile ->
            ( { model | game = model.game |> Game.spawnTile pos tile }
            , Cmd.none
            )
                |> Random.constant

        Action.AddPoint ->
            model.score
                + 1
                |> (\score ->
                        ( { model
                            | score = score
                            , maxScore = max model.maxScore score
                          }
                        , Cmd.none
                        )
                   )
                |> Random.constant

        Action.Chain list ->
            list
                |> List.foldl
                    (\a ->
                        Random.andThen
                            (\( m, l ) ->
                                m
                                    |> applyAction a
                                    |> Random.map (Tuple.mapSecond (\it -> it :: l))
                            )
                    )
                    (Random.constant ( model, [] ))
                |> Random.map (Tuple.mapSecond Cmd.batch)

        Action.PlaySound sound ->
            ( model, playSound (Sound.toString sound) ) |> Random.constant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move { left } ->
            if model.actions == [] && model.overlay == Nothing then
                model.game
                    |> Game.move { left = left }
                    |> (\( game, actions ) ->
                            ( { model
                                | game = game
                                , actions = actions
                              }
                            , Cmd.none
                            )
                       )

            else
                ( { model | overlay = Nothing }, Cmd.none )

        NextAction ->
            case model.actions of
                head :: tail ->
                    { model | actions = tail }
                        |> applyAction head
                        |> (\gen -> Random.step gen model.seed)
                        |> (\( ( m, cmd ), seed ) ->
                                ( { m
                                    | seed = seed
                                  }
                                , cmd
                                )
                           )

                [] ->
                    ( model, Cmd.none )

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        CloseOverlay ->
            ( { model | overlay = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Time.every 100 (\_ -> NextAction)
    , Browser.Events.onKeyUp
        (Json.Decode.string
            |> Json.Decode.field "key"
            |> Json.Decode.andThen
                (\string ->
                    case string of
                        "ArrowLeft" ->
                            { left = True }
                                |> Json.Decode.succeed

                        "ArrowRight" ->
                            { left = False }
                                |> Json.Decode.succeed

                        "a" ->
                            { left = True }
                                |> Json.Decode.succeed

                        "d" ->
                            { left = False }
                                |> Json.Decode.succeed

                        "A" ->
                            { left = True }
                                |> Json.Decode.succeed

                        "D" ->
                            { left = False }
                                |> Json.Decode.succeed

                        _ ->
                            Json.Decode.fail "not supported"
                )
            |> Json.Decode.map Move
        )
    ]
        |> Sub.batch


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
