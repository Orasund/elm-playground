module OracleCards.Main exposing (main)

import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import OracleCards.Card as Card exposing (Card(..))
import OracleCards.Image as Image
import OracleCards.Sigil as Sigil
import OracleCards.View as View
import Pixels
import Point2d
import Polyline2d
import Rectangle2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes


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

                Element _ ->
                    True

                Planet _ ->
                    True

                Emotion _ ->
                    True

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
    (Rectangle2d.from Point2d.origin (Point2d.pixels View.width View.height)
        |> Svg.rectangle2d
            [ Attributes.stroke "black"
            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 1
            , Attributes.fill <|
                if isWhite then
                    "white"

                else
                    View.blackBackground
            ]
    )
        :: (case card of
                Joker ->
                    []

                Trump _ ->
                    {- [ {--n
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
                         --viewValue

                       --]
                    -}
                    []

                {--{ value = n
                    , size = 4
                    }
                        |> Sigil.view (Point2d.pixels (View.width / 2) View.padding)--}
                Emotion _ ->
                    {--[ Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + View.relative 1))
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
                    ]--}
                    {--[ View.regularPolygon
                        { n = 3, scale = View.relative 2, standing = True }
                        ( View.width / 2, View.height - View.padding + View.relative 1 )
                        |> Svg.polygon2d
                            [ Attributes.stroke <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill "transparent"
                            ]
                    , Circle2d.atPoint (Point2d.pixels (View.width / 2) (View.height - View.padding + View.relative 1))
                        (Pixels.pixels <| View.relative 0.25)
                        |> Svg.circle2d
                            [ Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]
                    ]--}
                    []

                Element n ->
                    {--[ Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + (View.relative <| -1 + 1)))
                        (Pixels.pixels <|
                            View.relative <|
                                if n == 1 || n == 4 then
                                    3 / 4

                                else
                                    1
                        )
                        |> Svg.circle2d
                            [ Attributes.stroke <|
                                if n == 1 || n == 4 then
                                    "none"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill <|
                                if n == 1 || n == 4 then
                                    "white"

                                else
                                    View.blackBackground
                            ]
                    , Circle2d.atPoint (Point2d.pixels (View.padding + View.relative 1) (View.padding + (View.relative <| 3 - 1)))
                        (Pixels.pixels <|
                            View.relative <|
                                if n == 3 || n == 4 then
                                    3 / 4

                                else
                                    1
                        )
                        |> Svg.circle2d
                            [ Attributes.stroke <|
                                if n == 3 || n == 4 then
                                    "none"

                                else
                                    "white"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.5
                            , Attributes.fill <|
                                if n == 3 || n == 4 then
                                    "white"

                                else
                                    View.blackBackground
                            ]
                    ]--}
                    { value =
                        case n of
                            1 ->
                                2

                            2 ->
                                3

                            3 ->
                                1

                            4 ->
                                0

                            _ ->
                                0
                    , size = 2
                    }
                        |> Sigil.view (Point2d.pixels (View.width / 2) View.padding)

                Planet _ ->
                    {--[ View.regularPolygon
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
                    ]--}
                    { value = Card.value card - 1
                    , size = 3
                    }
                        |> Sigil.view (Point2d.pixels (View.width / 2) View.padding)

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
        ++ (case card of
                Planet _ ->
                    []

                Element _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels (View.padding / 2) (View.padding / 2 + View.radius)
                        , Point2d.pixels (View.padding / 2) (View.padding / 2)
                        , Point2d.pixels (View.padding / 2 + View.radius) (View.padding / 2)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - View.padding / 2 - View.radius) (View.padding / 2)
                        , Point2d.pixels (View.width - View.padding / 2) (View.padding / 2)
                        , Point2d.pixels (View.width - View.padding / 2) (View.padding / 2 + View.radius)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - View.padding / 2) (View.height - View.padding / 2 - View.radius)
                        , Point2d.pixels (View.width - View.padding / 2) (View.height - View.padding / 2)
                        , Point2d.pixels (View.width - View.padding / 2 - View.radius) (View.height - View.padding / 2)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.padding / 2 + View.radius) (View.height - View.padding / 2)
                        , Point2d.pixels (View.padding / 2) (View.height - View.padding / 2)
                        , Point2d.pixels (View.padding / 2) (View.height - View.padding / 2 - View.radius)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    ]

                _ ->
                    Rectangle2d.from
                        (Point2d.pixels (View.padding / 2) (View.padding / 2))
                        (Point2d.pixels (View.width - View.padding / 2) (View.height - View.padding / 2))
                        |> Svg.rectangle2d
                            [ Attributes.stroke <| Card.color card
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill <| "none"
                            ]
                        |> List.singleton
           )
        ++ Image.view card
        ++ [ card
                |> Card.description
                |> Svg.text
                |> List.singleton
                |> Svg.text_
                    [ Attributes.x <| String.fromFloat <| View.width / 2
                    , Attributes.y <| String.fromFloat <| View.height - View.padding - View.relative 3
                    , Attributes.textAnchor <| "middle"
                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative <| (3 / 70) * View.width) ++ "px sans-serif"
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
           , Rectangle2d.from Point2d.origin (Point2d.pixels View.width View.height)
                |> Svg.rectangle2d
                    [ Attributes.stroke "black"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 2
                    , Attributes.fill <| "transparent"
                    ]
           ]
        ++ (case card of
                Emotion _ ->
                    []

                _ ->
                    card
                        |> Card.title
                        |> Svg.text
                        |> List.singleton
                        |> Svg.text_
                            [ Attributes.x <| String.fromFloat <| View.width - View.padding
                            , Attributes.y <| String.fromFloat <| View.padding
                            , Attributes.textAnchor <| "start"
                            , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative <| (1.8 / 70) * View.width) ++ "px sans-serif"
                            , Attributes.writingMode <| "tb"
                            , Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]
                        |> List.singleton
           )


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
            ([ Element, Emotion ]
                |> List.concatMap (\fun -> List.range 1 4 |> List.map fun)
            )
        |> List.append
            ([ Planet ]
                |> List.concatMap (\fun -> List.range 1 8 |> List.map fun)
            )


main : Html msg
main =
    smallSet
        |> List.map
            (\card ->
                viewCard card
                    |> Svg.svg
                        [ Attributes.width <| (String.fromFloat <| View.zoom * View.width) ++ "px"
                        , Attributes.height <| (String.fromFloat <| View.zoom * View.height) ++ "px"
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
