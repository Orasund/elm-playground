module Mezzo.Main exposing (main)

import Array
import Browser
import Color
import Element exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import List.Extra as List
import Mezzo.Data.Card as Card
import Mezzo.Data.Game as Game exposing (Game)
import Mezzo.View.Card as Card
import Mezzo.View.Part as Part
import Mezzo.View.PartBubble as PartBubble
import Mezzo.View.Stack as Stack
import Queue
import Random exposing (Seed)
import Result.Extra as Result
import Stack
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography



----------------------
-- Model
----------------------


type State
    = Running
    | End


type Model
    = SettingUp
    | Ready
        { game : Game
        , state : State
        , seed : Seed
        }


type Msg
    = Play Int
    | Discard Int
    | Start Seed



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init () =
    ( SettingUp
    , Random.independentSeed
        |> Random.generate Start
    )


ready : Seed -> ( Model, Cmd Msg )
ready s =
    let
        ( game, seed ) =
            s
                |> Random.step Game.init
    in
    ( Ready
        { game = game
        , state = Running
        , seed = seed
        }
    , Cmd.none
    )



----------------------
-- Update
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case m of
        SettingUp ->
            case msg of
                Start s ->
                    ready s

                _ ->
                    ( m, Cmd.none )

        Ready model ->
            case ( msg, model.state ) of
                ( Play i, Running ) ->
                    case model.game |> Game.play i of
                        Ok m0 ->
                            ( Ready { model | game = m0 }, Cmd.none )

                        Err () ->
                            ( Ready { model | state = End }, Cmd.none )

                ( Discard i, Running ) ->
                    case model.game |> Game.discard i of
                        Ok m0 ->
                            ( Ready { model | game = m0 }, Cmd.none )

                        Err () ->
                            ( Ready { model | state = End }, Cmd.none )

                ( Start s, _ ) ->
                    ready s

                _ ->
                    ( m, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



----------------------
-- View
----------------------


view : Model -> List (Html Msg)
view m =
    case m of
        SettingUp ->
            []

        Ready model ->
            let
                ( last, middle, firstFew ) =
                    case
                        model.game.deck
                            |> Queue.toList
                            |> List.greedyGroupsOf 2
                    of
                        a :: b :: c :: ((d :: _) as tail) ->
                            case tail |> List.reverse of
                                x :: l ->
                                    ( x, l, [ c, b, a ] )

                                [] ->
                                    ( d, [], [ c, b, a ] )

                        [ a, b, c ] ->
                            ( c, [], [ b, a ] )

                        [ a, b ] ->
                            ( b, [], [ a ] )

                        [ a ] ->
                            ( a, [], [] )

                        [] ->
                            ( [], [], [] )

                ( discardFirst, discardTail ) =
                    model.game.discard
                        |> Stack.toList
                        |> List.splitAt 3
            in
            [ model.game.card
                |> Card.view Nothing
                |> Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                |> Element.el
                    [ Element.height <| Element.fill
                    , Element.width <| Element.fill
                    , Color.rgb255 240 240 240
                        |> Color.toRgba
                        |> Element.fromRgb
                        |> Background.color
                    ]
            , [ [ last |> Part.view |> List.singleton
                , if middle |> List.isEmpty then
                    []

                  else
                    [ Card.viewBackSmall
                    , middle
                        |> List.length
                        |> String.fromInt
                        |> Element.text
                        |> Element.el
                            [ Element.height <| Element.px 32
                            , Font.size 32
                            , Element.centerX
                            ]
                    ]
                        |> Element.column [ Element.spacing 10 ]
                        |> List.singleton
                , firstFew |> List.map Part.view
                ]
                    |> List.concat
                    |> Element.row
                        [ Element.spacing 4
                        , Element.alignLeft
                        ]
                    |> Element.el [ Element.width <| Element.fill ]
              , model.game.hand
                    |> Array.indexedMap
                        (\i ->
                            (Play i
                                |> Just
                                |> Card.view
                            )
                                >> (\el ->
                                        [ el
                                        , Widget.textButton
                                            (Material.outlinedButton Material.defaultPalette
                                                |> Customize.elementButton
                                                    [ Element.centerX ]
                                            )
                                            { text = "Put Aside"
                                            , onPress = Just <| Discard i
                                            }
                                        ]
                                            |> Element.column [ Element.spacing 5 ]
                                   )
                        )
                    |> Array.toList
                    |> List.reverse
                    |> Element.row [ Element.centerX, Element.spacing 10 ]
                    |> Element.el [ Element.width <| Element.fill ]
              , [ discardFirst |> List.map (Card.toParts >> PartBubble.viewJoined)
                , discardTail |> Stack.view |> List.singleton
                ]
                    |> List.concat
                    |> Element.column
                        [ Element.alignBottom
                        , Element.alignRight
                        ]
                    |> Element.el
                        [ Element.width <| Element.fill
                        , Element.height <| Element.fill
                        ]
              ]
                |> Element.row
                    [ Element.width <| Element.fill
                    , Element.padding 10
                    , Color.rgb255 200 200 200
                        |> Color.toRgba
                        |> Element.fromRgb
                        |> Background.color
                    ]
            ]
                |> Element.column
                    [ Element.height <| Element.fill
                    , Element.width <| Element.fill
                    ]
                |> Element.layout
                    (Typography.body1
                        ++ (case model.state of
                                End ->
                                    Widget.singleModal
                                        [ { onDismiss = Just <| Start model.seed
                                          , content =
                                                let
                                                    n =
                                                        model.game.discard
                                                            |> Stack.toList
                                                            |> List.filterMap
                                                                (\card ->
                                                                    case card.suit of
                                                                        ( a, Nothing ) ->
                                                                            Just (Card.suitToString a)

                                                                        _ ->
                                                                            Nothing
                                                                )
                                                            |> List.unique
                                                in
                                                [ Widget.fullBleedItem
                                                    (Material.fullBleedItem Material.defaultPalette
                                                        |> Customize.element Typography.h6
                                                    )
                                                    { text = "Game Over"
                                                    , onPress = Nothing
                                                    , icon = always Element.none
                                                    }
                                                , Widget.multiLineItem
                                                    (Material.multiLineItem Material.defaultPalette
                                                        |> Customize.element [ Element.height <| Element.shrink ]
                                                    )
                                                    { title =
                                                        "You have collected "
                                                            ++ (n
                                                                    |> List.length
                                                                    |> String.fromInt
                                                               )
                                                            ++ " out of 8 different full colored cards:"
                                                    , content = always Element.none
                                                    , text =
                                                        n
                                                            |> String.join ", "
                                                    , onPress = Nothing
                                                    , icon =
                                                        \{ size } ->
                                                            n
                                                                |> List.length
                                                                |> String.fromInt
                                                                |> Element.text
                                                                |> Element.el [ Font.size size, Element.centerX ]
                                                    }
                                                , Widget.button
                                                    (Material.containedButton Material.defaultPalette
                                                        |> Customize.elementButton [ Element.alignRight ]
                                                    )
                                                    { text = "Play Again"
                                                    , icon = always Element.none
                                                    , onPress = Just <| Start model.seed
                                                    }
                                                    |> Widget.asItem
                                                ]
                                                    |> Widget.itemList
                                                        (Material.cardColumn Material.defaultPalette
                                                            |> Customize.elementColumn
                                                                [ Element.width <| Element.px 400
                                                                , Element.centerX
                                                                , Element.centerY
                                                                ]
                                                            |> Customize.mapContent
                                                                (Customize.element
                                                                    [ Element.width <| Element.px 400
                                                                    , Element.height <| Element.shrink
                                                                    ]
                                                                )
                                                        )
                                          }
                                        ]

                                Running ->
                                    []
                           )
                    )
                |> List.singleton



----------------------
-- Main
----------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "Mezzo"
                , body =
                    view model
                }
        , update = update
        , subscriptions = subscriptions
        }
