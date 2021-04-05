module HermeticMind.CelticBread exposing (..)

import Angle
import Arc2d
import Direction2d
import Geometry.Svg as Svg
import HermeticMind.Data.Turtle as Turtle exposing (Turtle)
import Html exposing (Html)
import LineSegment2d
import Point2d
import Quantity exposing (Quantity(..))
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


zoom : number
zoom =
    8


config : { radius : Float, size : Float, strokeWidth : Float }
config =
    let
        strokeWidth =
            1
    in
    { radius = strokeWidth
    , size = strokeWidth * 28
    , strokeWidth = strokeWidth
    }


overshoot =
    0.05


turtle strokeWidth position direction =
    { position = position
    , direction = direction
    , lineFun =
        \{ from, to } ->
            let
                dir =
                    Direction2d.from from to
                        |> Maybe.withDefault Direction2d.positiveX

                len =
                    Point2d.distanceFrom from to

                vec =
                    Vector2d.withLength (len |> Quantity.plus (Quantity.unsafe overshoot)) dir

                segment =
                    LineSegment2d.from from (from |> Point2d.translateBy vec)
            in
            [ segment
                |> Svg.lineSegment2d
                    [ Attributes.stroke "black"
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    , arcFun =
        \{ around, by, from } ->
            let
                arc =
                    Arc2d.sweptAround around
                        (by
                            |> Angle.inRadians
                            |> (\r ->
                                    if r < 0 then
                                        r - overshoot

                                    else
                                        r + overshoot
                               )
                            |> Angle.radians
                        )
                        from
            in
            [ arc
                |> Svg.arc2d
                    [ Attributes.fill <| "none"
                    , Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    }


heart : Html msg
heart =
    let
        { radius, size, strokeWidth } =
            config
    in
    turtle strokeWidth
        (Point2d.unsafe { x = size / 2, y = size - radius * 2 })
        (pi
            |> Angle.radians
            |> Direction2d.fromAngle
        )
        |> Turtle.arcRightBy
            { angle = Angle.radians <| pi * 3 / 4
            , radius = radius * 2
            }
        |> Turtle.andThen (Turtle.forwardBy <| radius * 10)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = radius * 4
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * 2)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = radius * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * 2)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = radius * 4
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * 10)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 4
                , radius = radius * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| 2 * pi
                , radius = radius * 12
                }
            )
        |> Tuple.second
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


cross : Html msg
cross =
    let
        { size, strokeWidth, radius } =
            config

        rot =
            radius * 25 / 8

        lineLenght =
            rot * 4
    in
    turtle strokeWidth
        (Point2d.unsafe { x = size / 2, y = radius * 2 })
        (0
            |> Angle.radians
            |> Direction2d.fromAngle
        )
        |> Turtle.arcRightBy
            { angle = Angle.radians <| pi * 3 / 4
            , radius = rot
            }
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 4
                , radius = rot
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| 2 * pi
                , radius = radius * 12
                }
            )
        |> Tuple.second
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


hashtag : Html msg
hashtag =
    let
        { size, strokeWidth, radius } =
            config

        rot =
            radius * 73 / 32

        lineLenght =
            rot * 6
    in
    turtle strokeWidth
        (Point2d.unsafe { x = size / 2, y = radius * 2 })
        (0
            |> Angle.radians
            |> Direction2d.fromAngle
        )
        |> Turtle.arcRightBy
            { angle = Angle.radians <| pi * 3 / 4
            , radius = rot
            }
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = rot
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| radius * lineLenght)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 4
                , radius = rot
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| 2 * pi
                , radius = radius * 12
                }
            )
        |> Tuple.second
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


main : Html msg
main =
    Html.div []
        [ heart
        , cross
        , hashtag
        ]
