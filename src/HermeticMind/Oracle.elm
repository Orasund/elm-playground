module HermeticMind.Oracle exposing (..)

import Angle
import Browser
import Css
import Dict exposing (Dict)
import Direction2d
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Geometry.Svg as Svg
import HermeticMind.Data.Card as Card exposing (Card)
import HermeticMind.View.BinarySigil as BinarySigil
import HermeticMind.View.Card as Card
import Html exposing (Html)
import Point2d
import Random
import Random.List as Random
import Svg
import Svg.Attributes as SvgAttributes
import Time
import Vector2d


type alias Model =
    { offsetAngle : Float
    , cards : List Card
    , flipped : Dict Int ()
    }


type Msg
    = TimePassed Float
    | PressedGetCards
    | GotCards (List Card)
    | FlipCard Int


init : () -> ( Model, Cmd Msg )
init () =
    ( { offsetAngle = 0
      , cards = []
      , flipped = Dict.empty
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ms =
            100
    in
    Time.every ms (always (TimePassed ms))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePassed _ ->
            ( { model
                | offsetAngle = model.offsetAngle + 0.005
              }
            , Cmd.none
            )

        PressedGetCards ->
            ( { model | flipped = Dict.empty }
            , Random.generate GotCards
                (Random.choices 3 Card.asList
                    |> Random.map Tuple.first
                )
            )

        GotCards cards ->
            ( { model | cards = cards }, Cmd.none )

        FlipCard i ->
            ( { model
                | flipped =
                    model.flipped
                        |> Dict.update i (always <| Just ())
              }
            , Cmd.none
            )


viewCircle : { offset : Float, n : Int, size : Float, radius : Float, strokeWidth : Float } -> Html Msg
viewCircle { offset, n, size, radius, strokeWidth } =
    List.range 0 (2 ^ n - 1)
        |> List.concatMap
            (\r ->
                let
                    angle =
                        Angle.radians <| (2 * pi / toFloat (2 ^ n)) * (0.5 + toFloat r) + offset
                in
                { value = r
                , size = n
                , color = "black"
                , radius = strokeWidth * 2
                , strokeWidth = strokeWidth
                , point =
                    Point2d.pixels (size / 2) (size / 2)
                        |> Point2d.translateBy (Vector2d.pixels radius 0)
                        |> Point2d.rotateAround (Point2d.pixels (size / 2) (size / 2))
                            angle
                , direction =
                    angle
                        |> Direction2d.fromAngle
                        |> Direction2d.rotateClockwise

                --Direction2d.positiveX --
                }
                    |> BinarySigil.view
            )
        |> Svg.svg
            [ SvgAttributes.width <| (String.fromFloat <| size) ++ "px"
            , SvgAttributes.height <| (String.fromFloat <| size) ++ "px"
            , SvgAttributes.version <| "1.1"
            , SvgAttributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat size
                    ++ " "
                    ++ String.fromFloat size
            ]


view : Model -> Html Msg
view model =
    let
        size =
            800

        radius =
            350

        diversion =
            0.5

        cardZoom =
            0.3
    in
    (case model.cards of
        [] ->
            Input.button
                [ Font.size 32
                , Border.width <| 0
                , Element.centerX
                , Element.centerY
                , Font.color <| Element.rgb255 255 255 255
                , Font.shadow
                    { offset = ( 0, 0 )
                    , blur = 32
                    , color = Element.rgb255 0 0 255
                    }
                ]
                { onPress = PressedGetCards |> Just
                , label =
                    "Orakel befragen"
                        |> Element.text
                }

        list ->
            [ Input.button
                [ Font.size 32
                , Border.width <| 0
                , Font.color <| Element.rgb255 255 255 255
                , Font.shadow
                    { offset = ( 0, 0 )
                    , blur = 32
                    , color = Element.rgb255 0 0 255
                    }
                , Element.padding 16
                ]
                { onPress = PressedGetCards |> Just
                , label =
                    "Wiederholen"
                        |> Element.text
                }
            , list
                |> List.indexedMap
                    (\i card ->
                        Input.button
                            [ Element.height <| Element.px <| round <| cardZoom * Card.height + 2
                            , Element.width <| Element.px <| round <| cardZoom * Card.width + 2
                            , Border.width 1
                            , Border.shadow
                                { offset = ( 0, 0 )
                                , size = 4
                                , blur = 32
                                , color = Element.rgb255 0 0 127
                                }
                            ]
                            { onPress = Just <| FlipCard i
                            , label =
                                (case model.flipped |> Dict.get i of
                                    Just () ->
                                        card

                                    Nothing ->
                                        Card.back
                                )
                                    |> Card.view
                                    |> Svg.svg
                                        [ SvgAttributes.width <| (String.fromInt <| round <| cardZoom * Card.width) ++ "px"
                                        , SvgAttributes.height <| (String.fromInt <| round <| cardZoom * Card.height) ++ "px"
                                        , SvgAttributes.version <| "1.1"
                                        , SvgAttributes.viewBox <|
                                            "0 0 "
                                                ++ String.fromFloat Card.width
                                                ++ " "
                                                ++ String.fromFloat Card.height
                                        ]
                                    |> Element.html
                            }
                    )
                |> Element.row
                    [ Element.spacing 16
                    ]
            , Element.none |> Element.el []
            ]
                |> Element.column
                    [ Element.centerX
                    , Element.spaceEvenly
                    ]
    )
        |> Element.el
            [ Element.centerX
            , Element.centerY
            ]
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            , viewCircle
                { offset = model.offsetAngle
                , n = 4
                , size = size
                , radius = radius
                , strokeWidth = 2
                }
                |> Element.html
                |> Element.el
                    [ viewCircle
                        { offset = model.offsetAngle * (1 + diversion)
                        , n = 3
                        , size = size
                        , radius = radius
                        , strokeWidth = 2
                        }
                        |> Element.html
                        |> Element.el
                            [ viewCircle
                                { offset = -model.offsetAngle * (1 + diversion * 2)
                                , n = 2
                                , size = size
                                , radius = radius
                                , strokeWidth = 2
                                }
                                |> Element.html
                                |> Element.el
                                    [ viewCircle
                                        { offset = -model.offsetAngle * (1 + diversion * 3)
                                        , n = 1
                                        , size = size
                                        , radius = radius
                                        , strokeWidth = 2
                                        }
                                        |> Element.html
                                        |> Element.inFront
                                    , Element.centerX
                                    , Element.centerY
                                    ]
                                |> Element.inFront
                            , Element.centerX
                            , Element.centerY
                            ]
                        |> Element.inFront
                    , Element.centerX
                    , Element.centerY
                    ]
                |> Element.behindContent
            , Background.color <|
                Element.rgb255 ((round <| sin (model.offsetAngle * 25) * 32) + 48)
                    0
                    ((round <| sin (model.offsetAngle * 30) * 8) + 48)
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view =
            view
        , update = update
        , subscriptions = subscriptions
        }
