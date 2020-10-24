module OracleCards.Main exposing (main)

import Angle exposing (Angle)
import Circle2d exposing (Circle2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length)
import OracleCards.Card as Card exposing (Card(..))
import OracleCards.Image as Image
import OracleCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


viewCard : Card -> List (Svg msg)
viewCard card =
    let
        isWhite =
            case card of
                Black _ ->
                    False

                White _ ->
                    True

                Trump _ ->
                    True

                Joker ->
                    True

                _ ->
                    False

        viewValue =
            Card.value card
                |> String.fromInt
                |> Svg.text
                |> List.singleton
                |> Svg.text_
                    [ Attributes.x <| String.fromFloat <| View.padding
                    , Attributes.y <| String.fromFloat <| View.padding + View.relative 3
                    , Attributes.textAnchor <| "middle"
                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative 5) ++ "px sans-serif"
                    , Attributes.fill <|
                        Card.color card
                    ]
    in
    [ Rectangle2d.from Point2d.origin (Point2d.pixels View.width View.height)
        |> Svg.rectangle2d
            [ Attributes.stroke "black"
            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
            , Attributes.fill <|
                if isWhite then
                    "white"

                else
                    View.blackBackground
            ]
    ]
        ++ (case card of
                Joker ->
                    []

                Trump n ->
                    [ {--n
                        |> String.fromInt
                        |> Svg.text
                        |> List.singleton
                        |> Svg.text_
                            [ Attributes.x <| String.fromFloat <| View.padding
                            , Attributes.y <| String.fromFloat <| View.padding + View.relative 6.5
                            , Attributes.textAnchor <| "middle"
                            , Attributes.style <| "font: bold " ++ (String.fromFloat <| View.relative 2.5) ++ "px serif"
                            , Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]
                    , --}
                      viewValue
                    ]

                Emotion _ ->
                    [ Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + View.relative 1))
                        (Pixels.pixels <| View.relative 2)
                        |> Svg.circle2d
                            [ Attributes.stroke <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill <|
                                if isWhite then
                                    "white"

                                else
                                    View.blackBackground
                            ]
                    , Polygon2d.singleLoop
                        [ Point2d.pixels (View.padding - View.relative 1) (View.padding + View.relative 1)
                        , Point2d.pixels (View.padding + View.relative 3) (View.padding + View.relative 1)
                        ]
                        |> Svg.polygon2d
                            [ Attributes.stroke <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            ]
                    ]

                Season n ->
                    [ Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + (View.relative <| -1 + 3 / 4)))
                        (Pixels.pixels <| View.relative <| 3 / 4)
                        |> Svg.circle2d
                            [ Attributes.stroke <|
                                if n == 1 || n == 4 then
                                    View.blackBackground

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill <|
                                if n == 1 || n == 4 then
                                    "white"

                                else
                                    View.blackBackground
                            ]
                    , Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + (View.relative <| 3 - 3 / 4)))
                        (Pixels.pixels <| View.relative <| 3 / 4)
                        |> Svg.circle2d
                            [ Attributes.stroke <|
                                if n == 3 || n == 4 then
                                    View.blackBackground

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill <|
                                if n == 3 || n == 4 then
                                    "white"

                                else
                                    View.blackBackground
                            ]
                    , Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + View.relative 1))
                        (Pixels.pixels <| View.relative 2)
                        |> Svg.circle2d
                            [ Attributes.stroke <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill <| "none"
                            ]
                    ]

                Planet n ->
                    [ View.regularPolygon
                        { n = 4, scale = View.relative 2, standing = True }
                        ( View.padding + View.relative 1, View.padding + View.relative 1 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill "transparent"
                            ]
                    , Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + View.relative 1))
                        (Pixels.pixels <| View.relative 0.25)
                        |> Svg.circle2d
                            [ Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]
                    ]

                _ ->
                    if Card.value card == 7 then
                        []

                    else
                        [ Circle2d.atPoint (Point2d.pixels View.padding (View.padding + View.relative 6))
                            (Pixels.pixels <| View.relative 1)
                            |> Svg.circle2d
                                [ Attributes.stroke <|
                                    if isWhite then
                                        "black"

                                    else
                                        "white"
                                , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                                , Attributes.fill <|
                                    if isWhite then
                                        "white"

                                    else
                                        View.blackBackground
                                ]
                        , viewValue
                        ]
           )
        ++ [ Rectangle2d.from
                (Point2d.pixels (View.padding / 2) (View.padding / 2))
                (Point2d.pixels (View.width - View.padding / 2) (View.height - View.padding / 2))
                |> Svg.rectangle2d
                    [ Attributes.stroke <| Card.color card
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                    , Attributes.fill <| "none"
                    ]
           ]
        ++ Image.view card
        ++ [ card
                |> Card.description
                |> Svg.text
                |> List.singleton
                |> Svg.text_
                    [ Attributes.x <| String.fromFloat <| View.width / 2
                    , Attributes.y <| String.fromFloat <| View.height - View.padding - View.relative 3
                    , Attributes.textAnchor <| "middle"
                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative 3) ++ "px sans-serif"
                    , Attributes.fill <|
                        case card of
                            Black 2 ->
                                "black"

                            White 2 ->
                                "white"

                            _ ->
                                if isWhite then
                                    "black"

                                else
                                    "white"
                    ]
           , card
                |> Card.title
                |> Svg.text
                |> List.singleton
                |> Svg.text_
                    [ Attributes.x <| String.fromFloat <| View.width - View.padding
                    , Attributes.y <| String.fromFloat <| View.padding
                    , Attributes.textAnchor <| "start"
                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative 1) ++ "px sans-serif"
                    , Attributes.writingMode <| "tb"
                    , Attributes.fill <|
                        if isWhite then
                            "black"

                        else
                            "white"
                    ]
           , Rectangle2d.from Point2d.origin (Point2d.pixels View.width View.height)
                |> Svg.rectangle2d
                    [ Attributes.stroke "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 2
                    , Attributes.fill <| "transparent"
                    ]
           ]


smallSet : List Card
smallSet =
    [ White 7, Black 7, Joker ]
        {- [ White, Black ]
           |> List.concatMap
               (\fun ->
                   List.range 1 7
                       |> List.map fun
               )
        -}
        |> List.append
            (List.range 1 21
                |> List.map Trump
            )
        |> List.append
            ([ Season, Emotion ]
                |> List.concatMap (\fun -> List.range 1 4 |> List.map fun)
            )
        |> List.append
            ([ Planet ]
                |> List.concatMap (\fun -> List.range 1 8 |> List.map fun)
            )


main : Html msg
main =
    [ Season 1
    , Season 2
    , Season 3
    , Season 4
    , Planet 1
    , Planet 2
    , Planet 3
    , Planet 4
    , Planet 5
    , Planet 6
    , Planet 7
    , Planet 8
    , Emotion 1
    , Emotion 2
    , Emotion 3
    , Emotion 4
    , White 7
    , Black 7
    , Joker
    ]
        |> List.map
            (\card ->
                viewCard card
                    |> Svg.svg
                        [ Attributes.width <| String.fromFloat <| View.zoom * View.width
                        , Attributes.height <| String.fromFloat <| View.zoom * View.height
                        , Attributes.viewBox <|
                            "0 0 "
                                ++ String.fromFloat View.width
                                ++ " "
                                ++ String.fromFloat View.height
                        ]
            )
        |> Html.div []



{--
--}
