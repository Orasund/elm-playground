module Mezzo.Main exposing (main)

import Action
import Array
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Color
import Element exposing (Element, Option, el)
import Element.Background as Background
import Element.Font as Font
import Framework
import Html
import Html.Attributes as Attributes
import List.Extra as List
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Mezzo.Data.Card as Card exposing (Card, Suit)
import Mezzo.Data.Game as Game exposing (Game)
import Mezzo.View.Card as Card
import Mezzo.View.Part as Part
import Queue
import Random
import Result.Extra as Result
import Task
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography



----------------------
-- Model
----------------------


type State
    = Running
    | Won


type alias Model =
    { game : Game
    , state : State
    }


type Msg
    = Play Int
    | Discard Int



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game =
            42
                |> Random.initialSeed
                |> Random.step Game.init
                |> Tuple.first
      , state = Running
      }
    , Cmd.none
    )



----------------------
-- Update
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( Play i, Running ) ->
            ( case model.game |> Game.play i of
                Ok m0 ->
                    { model | game = m0 }

                Err () ->
                    { model | state = Won }
            , Cmd.none
            )

        ( Discard i, Running ) ->
            ( case model.game |> Game.discard i of
                Ok m0 ->
                    { model | game = m0 }

                Err () ->
                    { model | state = Won }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



----------------------
-- View
----------------------


view : Model -> Element Msg
view model =
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
    , [ model.game.deck
            |> Queue.toList
            |> List.greedyGroupsOf 2
            |> List.reverse
            |> List.map Part.view
            |> Element.row
                [ Element.centerX
                , Element.spacing 4
                , Element.alignLeft
                ]
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
                                    { text = "Discard"
                                    , onPress = Just <| Discard i
                                    }
                                ]
                                    |> Element.column [ Element.spacing 5 ]
                           )
                )
            |> Array.toList
            |> List.reverse
            |> Element.row [ Element.centerX, Element.spacing 10 ]
      , Element.none
      ]
        |> Element.row
            [ Element.spaceEvenly
            , Element.width <| Element.fill
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
                        |> Element.layout Typography.body1
                        |> List.singleton
                }
        , update = update
        , subscriptions = subscriptions
        }
