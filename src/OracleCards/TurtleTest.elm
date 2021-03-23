module OracleCards.TurtleTest exposing (..)

import Angle exposing (Angle)
import Arc2d
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import OracleCards.Data.Turtle as Turtle exposing (Turtle)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Five, Index, OnePlus, TwentyPlus)
import StaticArray.Length as Length exposing (Length)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


strokeWidth =
    1


lineWidth =
    4


size =
    200


zoom =
    4


main : Html msg
main =
    let
        ( turtle, drawing ) =
            { position = Point2d.unsafe { x = size / 2, y = size / 2 }
            , direction = Direction2d.positiveX
            , lineFun =
                \{ from, to } ->
                    let
                        segment =
                            LineSegment2d.from from to
                    in
                    [ segment
                        |> Svg.lineSegment2d
                            [ Attributes.stroke "black"
                            , Attributes.strokeWidth <| String.fromFloat <| lineWidth
                            ]
                    , segment
                        |> Svg.lineSegment2d
                            [ Attributes.stroke "white"
                            , Attributes.strokeWidth <| String.fromFloat <| lineWidth - 2 * strokeWidth
                            , Attributes.strokeLinecap "round"
                            ]
                    ]
            , arcFun =
                \{ around, by, from } ->
                    let
                        arc =
                            Arc2d.sweptAround around by from
                    in
                    [ arc
                        |> Svg.arc2d
                            [ Attributes.fill <| "none"
                            , Attributes.stroke <| "black"
                            , Attributes.strokeWidth <| String.fromFloat <| lineWidth
                            ]
                    , arc
                        |> Svg.arc2d
                            [ Attributes.fill <| "none"
                            , Attributes.stroke <| "white"
                            , Attributes.strokeWidth <| String.fromFloat <| lineWidth - 2 * strokeWidth
                            ]
                    ]
            }
                |> Turtle.rotateClockwise
                    { to = Direction2d.positiveY
                    , radius = 10
                    }
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateClockwise
                        { to = Direction2d.negativeX
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateClockwise
                        { to = Direction2d.negativeY
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateClockwise
                        { to = Direction2d.negativeY
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 40)
                |> Turtle.andThen
                    (Turtle.rotateCounterclockwise
                        { to = Direction2d.positiveY
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateCounterclockwise
                        { to = Direction2d.negativeX
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateCounterclockwise
                        { to = Direction2d.negativeY
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateCounterclockwise
                        { to = Direction2d.positiveY
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 20)
                |> Turtle.andThen
                    (Turtle.rotateCounterclockwise
                        { to = Direction2d.positiveX
                        , radius = 10
                        }
                    )
                |> Turtle.andThen (Turtle.forwardBy 60)
    in
    drawing
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| zoom * size) ++ "px"
            , Attributes.height <| (String.fromFloat <| zoom * size) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat size
                    ++ " "
                    ++ String.fromFloat size
            ]
