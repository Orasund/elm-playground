module HermeticMind.View.GreekMagicSymbol exposing (fromChar)

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
    1



--64


config : Float -> { eps : Float, unit : Float, size : Float, strokeWidth : Float }
config size =
    let
        unit =
            size / 9
    in
    { eps = 0.001
    , unit = unit
    , size = size
    , strokeWidth = unit
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


begin : Float -> Turtle (List (Svg msg)) -> ( Turtle (List (Svg msg)), List (Svg msg) )
begin strokeWidth =
    Turtle.rotateRightBy (Angle.radians <| pi / 2)
        >> Turtle.arcRightBy
            { angle = Angle.radians <| 0
            , radius = strokeWidth
            }
        >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))


end : Float -> ( Turtle (List (Svg msg)), List (Svg msg) ) -> List (Svg msg)
end strokeWidth =
    Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
        >> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| 0
                , radius = strokeWidth
                }
            )
        >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
        >> Tuple.second


letterA : Float -> Html msg
letterA size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 5 + strokeWidth / 2, y = strokeWidth + strokeWidth / 2 })
        Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy (unit * 1))
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy (3 * unit + strokeWidth))
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| 0
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> end strokeWidth
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


letterB : Float -> Html msg
letterB size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = size - unit - strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> end strokeWidth
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


letterC : Float -> Html msg
letterC size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| unit)
        |> end strokeWidth
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


letterD : Float -> Html msg
letterD size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 5 + strokeWidth / 2, y = unit + strokeWidth / 2 })
        Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterE : Float -> Html msg
letterE size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 3 + strokeWidth / 2, y = unit + strokeWidth / 2 })
        Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterF : Float -> Html msg
letterF size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterG : Float -> Html msg
letterG size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 5 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> end strokeWidth
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


letterH : Float -> Html msg
letterH size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 6 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> end strokeWidth
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


letterI : Float -> Html msg
letterI size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth
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


letterJ : Float -> Html msg
letterJ size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> end strokeWidth
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


letterK : Float -> Html msg
letterK size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth
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


letterL : Float -> Html msg
letterL size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth
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


letterM : Float -> Html msg
letterM size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 7 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 7 * unit + strokeWidth / 2)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> end strokeWidth
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


letterN : Float -> Html msg
letterN size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 1 + strokeWidth / 2, y = unit * 4 + strokeWidth / 2 })
        Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> end strokeWidth
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


letterO : Float -> Html msg
letterO size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 4 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> end strokeWidth
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


letterP : Float -> Html msg
letterP size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterQ : Float -> Html msg
letterQ size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 1 + strokeWidth / 2, y = unit * 2 + strokeWidth / 2 })
        Direction2d.positiveY
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi * 3 / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit)
        |> end strokeWidth
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


letterR : Float -> Html msg
letterR size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2.5
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterS : Float -> Html msg
letterS size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 5 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.negativeX
        |> begin strokeWidth
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> end strokeWidth
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


letterT : Float -> Html msg
letterT size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 3 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterU : Float -> Html msg
letterU size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 4 * unit)
        |> end strokeWidth
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


letterV : Float -> Html msg
letterV size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 4 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1.5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2.5
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 0.5 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 4
                }
            )
        |> end strokeWidth
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


letterW : Float -> Html msg
letterW size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 4 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 2.25 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1.75
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1.75
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1.25 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 4
                }
            )
        |> end strokeWidth
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


letterX : Float -> Html msg
letterX size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 6 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 2.5
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi
                , radius = unit * 1
                }
            )
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 4
                }
            )
        |> end strokeWidth
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


letterY : Float -> Html msg
letterY size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = eps
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 7 * unit + strokeWidth / 2)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen (Turtle.forwardBy <| 2 * unit + strokeWidth / 2)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 3
                }
            )
        |> end strokeWidth
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


letterZ : Float -> Html msg
letterZ size =
    let
        { eps, unit, strokeWidth } =
            config size
    in
    turtle strokeWidth
        (Point2d.unsafe { x = unit * 2 + strokeWidth / 2, y = unit * 1 + strokeWidth / 2 })
        Direction2d.positiveX
        |> begin strokeWidth
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi))
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit * 2
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 1 * unit)
        |> Turtle.andThen
            (Turtle.arcLeftBy
                { angle = Angle.radians <| pi / 2
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> Turtle.andThen
            (Turtle.arcRightBy
                { angle = Angle.radians <| pi
                , radius = unit
                }
            )
        |> Turtle.andThen (Turtle.forwardBy <| 3 * unit)
        |> end strokeWidth
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


fromChar : Float -> Char -> Maybe (Html msg)
fromChar size char =
    case char |> Char.toLower of
        'a' ->
            Just <| letterA size

        'b' ->
            Just <| letterB size

        'c' ->
            Just <| letterC size

        'd' ->
            Just <| letterD size

        'e' ->
            Just <| letterE size

        'f' ->
            Just <| letterF size

        'g' ->
            Just <| letterG size

        'h' ->
            Just <| letterH size

        'i' ->
            Just <| letterI size

        'j' ->
            Just <| letterJ size

        'k' ->
            Just <| letterK size

        'l' ->
            Just <| letterL size

        'm' ->
            Just <| letterM size

        'n' ->
            Just <| letterN size

        'o' ->
            Just <| letterO size

        'p' ->
            Just <| letterP size

        'q' ->
            Just <| letterQ size

        'r' ->
            Just <| letterR size

        's' ->
            Just <| letterS size

        't' ->
            Just <| letterT size

        'u' ->
            Just <| letterU size

        'v' ->
            Just <| letterV size

        'w' ->
            Just <| letterW size

        'x' ->
            Just <| letterX size

        'y' ->
            Just <| letterY size

        'z' ->
            Just <| letterZ size

        _ ->
            Nothing
