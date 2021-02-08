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
                Binary 1 ->
                    False

                Back ->
                    False

                _ ->
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
            [ Attributes.stroke "none"
            , Attributes.strokeWidth <| String.fromFloat <| 0
            , Attributes.fill <|
                if isWhite then
                    "white"

                else
                    View.blackBackground
            ]
    )
        :: (case card of
                Joker ->
                    { value = 0
                    , size = 0
                    , color = "black"
                    , radius = View.relative <| 1 / 2
                    , strokeWidth = View.relative <| 1 / 8
                    , point = Point2d.pixels (View.width / 2) View.padding
                    }
                        |> Sigil.view

                Trump _ ->
                    []

                Element n ->
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
                    , color = "black"
                    , radius = View.relative <| 1 / 2
                    , strokeWidth = View.relative <| 1 / 8
                    , point = Point2d.pixels (View.width / 2) View.padding
                    }
                        |> Sigil.view

                Planet _ ->
                    { value = Card.value card - 1
                    , size = 3
                    , color = "black"
                    , radius = View.relative <| 1 / 2
                    , strokeWidth = View.relative <| 1 / 8
                    , point = Point2d.pixels (View.width / 2) View.padding
                    }
                        |> Sigil.view

                Binary n ->
                    case n of
                        0 ->
                            { value = n
                            , size = 1
                            , color = "black"
                            , radius = View.relative <| 1 / 2
                            , strokeWidth = View.relative <| 1 / 8
                            , point = Point2d.pixels (View.width / 2) View.padding
                            }
                                |> Sigil.view

                        1 ->
                            { value = n
                            , size = 1
                            , color = "white"
                            , radius = View.relative <| 1 / 2
                            , strokeWidth = View.relative <| 1 / 8
                            , point = Point2d.pixels (View.width / 2) View.padding
                            }
                                |> Sigil.view

                        _ ->
                            []

                Virtue _ ->
                    { value = Card.value card - 1
                    , size = 4
                    , color = "black"
                    , radius = View.relative <| 1 / 2
                    , strokeWidth = View.relative <| 1 / 8
                    , point = Point2d.pixels (View.width / 2) View.padding
                    }
                        |> Sigil.view

                _ ->
                    []
           )
        ++ (case card of
                Planet _ ->
                    []

                Element _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels 0 (View.padding + View.radius)
                        , Point2d.pixels 0 0
                        , Point2d.pixels (View.padding + View.radius) 0
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - View.padding - View.radius) 0
                        , Point2d.pixels View.width 0
                        , Point2d.pixels View.width (View.padding + View.radius)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels View.width (View.height - View.padding - View.radius)
                        , Point2d.pixels View.width View.height
                        , Point2d.pixels (View.width - View.padding - View.radius) View.height
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.padding + View.radius) View.height
                        , Point2d.pixels 0 View.height
                        , Point2d.pixels 0 (View.height - View.padding - View.radius)
                        ]
                        |> Svg.polyline2d
                            [ Attributes.stroke <| "none"
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill "black"
                            ]
                    ]

                Binary _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7)
                        , Point2d.pixels (12 * 7 + View.radius * 2) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7 - View.radius * 2) (12 * 7)
                        , Point2d.pixels (View.width - 12 * 7) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7) (12 * 7)
                        , Point2d.pixels (View.width - 12 * 7) (12 * 7 + View.radius * 2)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7 - View.radius * 2)
                        , Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7)
                        , Point2d.pixels (View.width - 12 * 7 - View.radius * 2) (View.height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7 + View.radius * 2) (View.height - 12 * 7)
                        , Point2d.pixels (12 * 7) (View.height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (View.height - 12 * 7)
                        , Point2d.pixels (12 * 7) (View.height - 12 * 7 - View.radius * 2)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7 + View.radius * 2)
                        , Point2d.pixels (12 * 7) (12 * 7)
                        ]
                    ]
                        |> List.map
                            (Svg.polyline2d
                                [ Attributes.stroke <| Card.color card
                                , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                                , Attributes.fill <| "none"
                                ]
                            )

                Trump _ ->
                    [ Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7)
                        , Point2d.pixels (View.width - 12 * 7) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7)
                        , Point2d.pixels (12 * 7) (View.height - 12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (View.height - 12 * 7)
                        , Point2d.pixels (12 * 7) (View.height - 12 * 7 - View.relative 1)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (12 * 7) (12 * 7 + View.relative 1)
                        , Point2d.pixels (12 * 7) (12 * 7)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7) (12 * 7)
                        , Point2d.pixels (View.width - 12 * 7) (12 * 7 + View.relative 1)
                        ]
                    , Polyline2d.fromVertices
                        [ Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7 - View.relative 1)
                        , Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7)
                        ]
                    ]
                        |> List.map
                            (Svg.polyline2d
                                [ Attributes.stroke <| Card.color card
                                , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                                , Attributes.fill <| "none"
                                , Attributes.strokeLinecap <| "square"
                                ]
                            )

                _ ->
                    Rectangle2d.from
                        (Point2d.pixels (12 * 7) (12 * 7))
                        (Point2d.pixels (View.width - 12 * 7) (View.height - 12 * 7))
                        |> Svg.rectangle2d
                            [ Attributes.stroke <| Card.color card
                            , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 0.1
                            , Attributes.fill <| "none"
                            ]
                        |> List.singleton
           )
        ++ Image.view card
        ++ ((card
                |> Card.description
                |> Svg.text
                |> List.singleton
                |> Svg.text_
                    [ Attributes.x <| String.fromFloat <| View.width / 2
                    , Attributes.y <| String.fromFloat <| View.height - View.padding - View.relative 3
                    , Attributes.textAnchor <| "middle"
                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative <| 3) ++ "px sans-serif"
                    , Attributes.fill <|
                        if isWhite then
                            "black"

                        else
                            "white"
                    ]
            )
                :: (card
                        |> Card.title
                        |> Svg.text
                        |> List.singleton
                        |> Svg.text_
                            [ Attributes.x <| String.fromFloat <| View.width - View.padding - (View.relative <| 0.6)
                            , Attributes.y <| String.fromFloat <| View.padding
                            , Attributes.textAnchor <| "start"
                            , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative <| 1.8) ++ "px sans-serif"
                            , Attributes.writingMode <| "tb"
                            , Attributes.fill <|
                                if isWhite then
                                    "black"

                                else
                                    "white"
                            ]
                        |> List.singleton
                   )
           )


smallSet : List Card
smallSet =
    [ Binary 0, Binary 1, Joker, Back ]
        |> List.append
            (List.range 1 16
                |> List.map Virtue
            )
        |> List.append
            ([ 1, 2, 5, 8, 9, 10, 12, 15, 18 ]
                |> List.map Trump
            )
        |> List.append
            ([ Element ]
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
                        , Attributes.version <| "1.1"
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
