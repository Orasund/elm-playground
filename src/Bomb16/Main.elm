module Bomb16.Main exposing (main)

{-|


# Dun16

by Lucas Payr

Created: 22.6.2021

-}

import Bomb16.Data.Cell as Cell exposing (Cell(..))
import Bomb16.Data.Game as Game exposing (Game)
import Bomb16.Data.Input as Input exposing (Direction)
import Bomb16.Data.World as World
import Bomb16.View.Grid as Grid
import Browser
import Browser.Events as Events
import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Font as Font
import Random exposing (Seed)
import Svg.Attributes exposing (direction)
import Time
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography


type alias Model =
    { game : Game
    , seed : Seed
    , process :
        Maybe
            { direction : Direction
            , index : Int
            }
    , gameOver : Bool
    }


type Msg
    = Move (Maybe Direction)
    | Tick
    | Restart


init : ( Model, Cmd Msg )
init =
    let
        ( game, seed ) =
            Random.step Game.init (Random.initialSeed 42)
    in
    ( { game = game
      , seed = seed
      , process = Nothing
      , gameOver = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move maybePos ->
            case maybePos of
                Just direction ->
                    case model.process of
                        Nothing ->
                            let
                                game =
                                    model.game
                            in
                            ( { model
                                | process =
                                    Just
                                        { direction = direction
                                        , index = 1
                                        }
                                , game = { game | updateThisTurn = False }
                              }
                            , Cmd.none
                            )

                        Just _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Tick ->
            case model.process of
                Just process ->
                    let
                        game =
                            model.game |> Game.update process

                        level =
                            game.world
                                |> World.find
                                    (\cell ->
                                        case cell of
                                            Sword _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                |> Maybe.andThen
                                    (\( _, cell ) ->
                                        case cell of
                                            Sword n ->
                                                Just n

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.withDefault 1
                    in
                    ( if process.index == World.size - 1 then
                        let
                            ( world, newSeed ) =
                                Random.step
                                    (game.world
                                        |> World.add
                                            { cell = Cell.generate
                                            , direction = process.direction
                                            }
                                    )
                                    model.seed
                        in
                        { model
                            | game = { game | world = world }
                            , seed = newSeed
                            , process = Nothing
                            , gameOver = { game | world = world } |> Game.isOver
                        }

                      else
                        { model
                            | game = game
                            , process = Just { process | index = process.index + 1 }
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Restart ->
            let
                ( game, seed ) =
                    Random.step Game.init model.seed
            in
            ( { game = game
              , seed = seed
              , process = Nothing
              , gameOver = False
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Events.onKeyDown (Input.decoder Move)
    , Time.every (1000 / 32) (always Tick)
    ]
        |> Sub.batch


view : Model -> Element Msg
view model =
    let
        modal : List (Attribute Msg)
        modal =
            { content =
                [ "Game Over"
                    |> Element.text
                    |> Element.el
                        (Typography.h6
                            ++ [ Element.centerX
                               , Element.centerY
                               ]
                        )
                    |> Widget.asItem
                , "Score"
                    |> Element.text
                    |> Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                    |> Widget.asItem
                , String.fromInt model.game.score
                    |> Element.text
                    |> Element.el
                        (Typography.h4
                            ++ [ Element.centerX
                               , Element.centerY
                               ]
                        )
                    |> Widget.asItem
                , Widget.button
                    (Material.containedButton Material.defaultPalette
                        |> Customize.elementButton [ Element.centerX ]
                    )
                    { text = "Restart"
                    , icon = always Element.none
                    , onPress = Just Restart
                    }
                    |> Widget.asItem
                ]
                    |> Widget.itemList
                        (Material.cardColumn Material.defaultPalette
                            |> Customize.elementColumn
                                [ Element.centerY
                                , Element.width <| Element.px 250
                                , Element.centerX
                                , Font.family [ Font.serif ]
                                ]
                            |> Customize.mapContent
                                (Customize.element [ Element.width <| Element.px 250 ])
                        )
            , onDismiss = Nothing
            }
                |> List.singleton
                |> Widget.singleModal
    in
    Widget.insetItem
        (Material.insetItem Material.defaultPalette
            |> Customize.element [ Element.width Element.fill ]
        )
        { text = ""
        , onPress = Nothing
        , icon =
            \{ size } ->
                [ "Score"
                    |> Element.text
                    |> Element.el
                        [ Font.size 10
                        , Element.alignBottom
                        ]
                , String.fromInt model.game.score
                    |> Element.text
                    |> Element.el
                        [ Font.size size
                        , Element.padding
                            0
                        ]
                ]
                    |> Element.row
                        [ Element.height <| Element.px <| size
                        , Element.spacing 4
                        , Font.family [ Font.serif ]
                        ]
        , content = always Element.none
        }
        :: (model.game.world
                |> Grid.view ""
           )
        ++ [ Widget.insetItem
                (Material.insetItem Material.defaultPalette
                    |> Customize.element
                        [ Element.width Element.fill
                        , Font.family [ Font.serif ]
                        ]
                )
                { text = ""
                , onPress = Nothing
                , icon =
                    always Element.none
                , content = always Element.none
                }
           ]
        |> Widget.itemList
            (Material.cardColumn Material.defaultPalette
                |> Customize.elementColumn
                    ([ Element.width <| Element.px 381
                     , Element.centerX
                     , Element.centerY
                     ]
                        ++ (if model.gameOver then
                                modal

                            else
                                []
                           )
                    )
                |> Customize.mapContent
                    (Customize.element
                        [ Element.padding 0
                        , Element.width <| Element.shrink
                        ]
                        >> Customize.ifFirst
                            [ Border.widthEach
                                { bottom = 1
                                , left = 1
                                , right = 1
                                , top = 1
                                }
                            ]
                        >> Customize.otherwise
                            [ Border.widthEach
                                { bottom = 1
                                , left = 1
                                , right = 1
                                , top = 0
                                }
                            ]
                    )
            )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view =
            view
                >> Element.layout
                    [ Font.family
                        [ Font.typeface "Noto Emoji"
                        ]
                    ]
        , update = update
        , subscriptions = subscriptions
        }
