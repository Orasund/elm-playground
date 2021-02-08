module Farmig.Main exposing (main)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Element exposing (Attribute, Element)
import Element.Border as Border
import Element.Font as Font
import Farmig.Data.Cell exposing (Cell)
import Farmig.Data.Game as Game exposing (Game)
import Farmig.Data.Input as Input
import Farmig.Data.Item as Item exposing (Item)
import Farmig.View.Achievement as Achievement
import Farmig.View.Grid as Grid
import Random exposing (Seed)
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography


type alias Model =
    { game : Game
    , seed : Seed
    }


type Msg
    = Move (Maybe ( Int, Int ))
    | Restart


init : ( Model, Cmd Msg )
init =
    let
        ( game, seed ) =
            Random.step Game.init (Random.initialSeed 42)
    in
    ( { game = game
      , seed = seed
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move maybePos ->
            case maybePos of
                Just pos ->
                    if model.game.food > 0 then
                        let
                            ( game, seed ) =
                                Random.step (model.game |> Game.update pos)
                                    model.seed
                        in
                        ( { game = game
                          , seed = seed
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Restart ->
            let
                ( game, seed ) =
                    Random.step Game.init model.seed
            in
            ( { game = game
              , seed = seed
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onKeyDown (Input.decoder Move)


view : Model -> Element Msg
view model =
    let
        modal : List (Attribute Msg)
        modal =
            { content =
                [ "You Died"
                    |> Element.text
                    |> Element.el
                        (Typography.h6
                            ++ [ Element.centerX
                               , Element.centerY
                               ]
                        )
                    |> Widget.asItem
                , "at level"
                    |> Element.text
                    |> Element.el [ Element.centerX, Element.padding 0 ]
                    |> Widget.asItem
                , model.game.level
                    |> String.fromInt
                    |> Element.text
                    |> Element.el
                        (Typography.h4
                            ++ [ Element.centerX
                               , Element.centerY
                               ]
                        )
                    |> Widget.asItem
                , Achievement.view model.game.achievement
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
                [ String.fromInt model.game.food
                    |> Element.text
                    |> Element.el
                        [ Font.size size
                        , Element.padding
                            0
                        ]
                , "Food"
                    |> Element.text
                    |> Element.el
                        [ Font.size 10
                        , Element.alignBottom
                        ]
                ]
                    |> Element.row
                        [ Element.height <| Element.px <| size
                        , Element.spacing 4
                        , Font.family [ Font.serif ]
                        ]
        , content =
            \{ size } ->
                [ "Level"
                    |> Element.text
                    |> Element.el
                        [ Font.size 10
                        , Element.alignBottom
                        ]
                , String.fromInt model.game.level
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
                        , Element.alignRight
                        ]
        }
        :: (model.game.world
                |> Grid.view
                    { player = model.game.player
                    , screenSize = 7
                    , food = model.game.food
                    , item = model.game.item
                    , tree =
                        if model.game.level > 24 then
                            "🌵"

                        else
                            "🌲"
                    }
           )
        ++ (model.game.item
                |> Maybe.map
                    (\item ->
                        [ Widget.multiLineItem
                            (Material.multiLineItem Material.defaultPalette
                                |> Customize.element
                                    [ Element.width Element.fill
                                    , Font.family [ Font.serif ]
                                    ]
                            )
                            { title = item |> Item.name
                            , text = item |> Item.description
                            , onPress = Nothing
                            , icon =
                                \{ size } ->
                                    item
                                        |> Item.toString
                                        |> Element.text
                                        |> Element.el
                                            [ Element.width <| Element.px size
                                            , Element.height <| Element.px size
                                            , Font.size size
                                            , Font.family
                                                [ Font.typeface "Noto Emoji"
                                                ]
                                            ]
                            , content = always Element.none
                            }
                        ]
                    )
                |> Maybe.withDefault
                    [ Widget.multiLineItem
                        (Material.multiLineItem Material.defaultPalette
                            |> Customize.element
                                [ Element.width Element.fill
                                , Font.family [ Font.serif ]
                                ]
                        )
                        { title = "No item picked up"
                        , text = "Items have a colored background."
                        , onPress = Nothing
                        , icon =
                            always Element.none
                        , content = always Element.none
                        }
                    ]
           )
        |> Widget.itemList
            (Material.cardColumn Material.defaultPalette
                |> Customize.elementColumn
                    ([ Element.width <| Element.px 358
                     , Element.centerX
                     , Element.centerY
                     ]
                        ++ (if model.game.food > 0 then
                                []

                            else
                                modal
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
