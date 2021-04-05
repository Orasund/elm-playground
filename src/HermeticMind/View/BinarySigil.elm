module HermeticMind.View.BinarySigil exposing (view)

import Angle
import Arc2d
import Binary
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import HermeticMind.Data.Turtle as Turtle exposing (Turtle)
import LineSegment2d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


type Direction
    = Up
    | Down


type Shape
    = Start
    | End
    | SemiCircle
    | Line
    | Loop
    | LineLoop
    | Spiral
    | Singleton


type alias Model msg =
    { drawing : Turtle (List (Svg msg)) -> ( Turtle (List (Svg msg)), List (Svg msg) )
    , size : Vector2d Float ()
    , direction : Direction
    }


singleton :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
singleton { radius, size, direction } =
    { drawing =
        Turtle.rotateLeftBy (Angle.radians <| pi / 2)
            >> Turtle.arcLeftBy { angle = Angle.radians 0, radius = radius / 2 }
            >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
            >> Turtle.andThen (Turtle.forwardBy radius)
            >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians 0, radius = radius / 2 })
            >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
    , size = size
    , direction = direction
    }


spiral :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
spiral { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.arcLeftBy { angle = Angle.radians 0, radius = radius / 2 }
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians pi, radius = radius * 2 })
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = -(radius * 4), y = 0 })
            , direction = Up
            }

        Down ->
            { drawing =
                Turtle.arcRightBy { angle = Angle.radians 0, radius = radius / 2 }
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians pi, radius = radius * 2 })
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius * 4, y = 0 })
            , direction = Down
            }


start :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
start { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.jumpForward (radius * 2)
                    >> Turtle.rotateLeftBy (Angle.radians <| pi / 2)
                    >> Turtle.jumpForward (radius * 2)
                    >> Turtle.init []
                    >> Turtle.andThen
                        (Turtle.arcRightBy
                            { angle = Angle.radians 0
                            , radius = radius / 2
                            }
                        )
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen
                        (Turtle.arcLeftBy
                            { angle = Angle.radians (pi / 2)
                            , radius = radius * 2
                            }
                        )
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            , size = size
            , direction = Up
            }

        Down ->
            { drawing =
                Turtle.init []
                    >> Turtle.andThen
                        (Turtle.arcRightBy
                            { angle = Angle.radians 0
                            , radius = radius / 2
                            }
                        )
            , size = size
            , direction = Down
            }


end :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
end { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.forwardBy radius
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians 0, radius = radius / 2 })
            , size = size
            , direction = Up
            }

        Down ->
            { drawing =
                Turtle.rotateRightBy (Angle.radians <| pi / 2)
                    >> Turtle.arcLeftBy { angle = Angle.radians <| pi / 2, radius = radius }
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| 0, radius = radius / 2 })
            , size = size
            , direction = Down
            }


semiCircle :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
semiCircle { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.rotateRightBy (Angle.radians <| pi / 2)
                    >> Turtle.init []
                    >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| pi, radius = radius })
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius * 2, y = 0 })
            , direction = Up
            }

        Down ->
            { drawing =
                Turtle.rotateLeftBy (Angle.radians <| pi / 2)
                    >> Turtle.init []
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi, radius = radius })
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius * 2, y = 0 })
            , direction = Down
            }


loop :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
loop { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.rotateRightBy (Angle.radians <| pi / 2)
                    >> Turtle.init []
                    >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| pi / 2, radius = radius * 1.5 })
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| 0, radius = radius / 2 })
                    >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| pi / 2, radius = radius * 1.5 })
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius * 3, y = 0 })
            , direction = Up
            }

        Down ->
            { drawing =
                Turtle.rotateLeftBy (Angle.radians <| pi / 2)
                    >> Turtle.init []
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi / 2, radius = radius * 1.5 })
                    >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| 0, radius = radius / 2 })
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi / 2, radius = radius * 1.5 })
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius * 3, y = 0 })
            , direction = Down
            }


line :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
line { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.init []
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen (Turtle.forwardBy (radius * 4))
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = 0, y = radius * 4 })
            , direction = Down
            }

        Down ->
            { drawing =
                Turtle.init []
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
                    >> Turtle.andThen (Turtle.forwardBy (radius * 4))
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = 0, y = -radius * 4 })
            , direction = Up
            }


lineLoop :
    { radius : Float
    , size : Vector2d Float ()
    , direction : Direction
    }
    -> Model msg
lineLoop { radius, size, direction } =
    case direction of
        Up ->
            { drawing =
                Turtle.rotateRightBy (Angle.radians <| pi / 2)
                    >> Turtle.init []
                    >> Turtle.andThen (Turtle.forwardBy (radius * 4))
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi, radius = radius / 2 })
                    >> Turtle.andThen (Turtle.arcRightBy { angle = Angle.radians <| pi, radius = radius })
                    >> Turtle.map (Turtle.rotateLeftBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius, y = radius * 4 })
            , direction = Down
            }

        Down ->
            { drawing =
                Turtle.rotateLeftBy (Angle.radians <| pi / 2)
                    >> Turtle.init []
                    >> Turtle.andThen (Turtle.forwardBy (radius * 4))
                    >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| pi, radius = radius / 2 })
                    >> Turtle.andThen (Turtle.arcLeftBy { angle = Angle.radians <| pi, radius = radius })
                    >> Turtle.map (Turtle.rotateRightBy (Angle.radians <| pi / 2))
            , size = size |> Vector2d.plus (Vector2d.unsafe { x = radius, y = -radius * 4 })
            , direction = Up
            }


shapeToSvg :
    { p : Vector2d Pixels ()
    , direction : Direction
    , radius : Float
    }
    -> Shape
    -> ( Turtle (List (Svg msg)) -> ( Turtle (List (Svg msg)), List (Svg msg) ), Vector2d Pixels (), Direction )
shapeToSvg { p, direction, radius } shape =
    let
        wrappingFun f =
            let
                out =
                    f
                        { radius = radius
                        , size =
                            p
                                |> Vector2d.toRecord Quantity.unwrap
                                |> Vector2d.unsafe
                        , direction = direction
                        }
            in
            ( --\offset ->
              --turtle
              {--|> Turtle.jumpTo
                        (offset
                            |> Point2d.translateBy p
                            |> Point2d.toRecord Quantity.unwrap
                            |> Point2d.unsafe
                        )--}
              out.drawing
            , out.size
                |> Vector2d.toRecord Quantity.unwrap
                |> Vector2d.unsafe
            , out.direction
            )
    in
    case shape of
        Singleton ->
            wrappingFun singleton

        Spiral ->
            wrappingFun spiral

        Start ->
            wrappingFun start

        End ->
            wrappingFun end

        SemiCircle ->
            wrappingFun semiCircle

        Loop ->
            wrappingFun loop

        Line ->
            wrappingFun line

        LineLoop ->
            wrappingFun lineLoop


shapeListToSvg :
    { p : Vector2d Float ()
    , direction : Direction
    , radius : Float
    }
    -> List Shape
    -> { drawing : Turtle (List (Svg msg)) -> ( Turtle (List (Svg msg)), List (Svg msg) ), size : Vector2d Float (), direction : Direction }
shapeListToSvg { p, direction, radius } =
    List.foldl
        (\a out ->
            let
                ( svg, newPoint, newDirection ) =
                    a
                        |> shapeToSvg
                            { p =
                                out.size
                                    |> Vector2d.toRecord Quantity.unwrap
                                    |> Vector2d.unsafe
                            , direction = out.direction
                            , radius = radius
                            }
            in
            { drawing = out.drawing >> Turtle.andThen svg
            , size = newPoint |> Vector2d.toRecord Quantity.unwrap |> Vector2d.unsafe
            , direction = newDirection
            }
        )
        { drawing =
            \turtle ->
                turtle
                    |> Turtle.rotateRightBy (Angle.radians <| pi)
                    |> Turtle.rotateRightBy (Angle.radians <| pi)
                    |> Turtle.init []
        , size = p |> Vector2d.toRecord Quantity.unwrap |> Vector2d.unsafe
        , direction = direction
        }


boolListToShapeList : List Bool -> ( Direction, List Shape )
boolListToShapeList l =
    let
        rec rem =
            case rem of
                [] ->
                    []

                [ _ ] ->
                    [ End ]

                [ True, False ] ->
                    [ Line, End ]

                [ False, True ] ->
                    [ Line, End ]

                True :: False :: ((True :: _) as tail) ->
                    Loop :: rec tail

                False :: True :: ((False :: _) as tail) ->
                    Loop :: rec tail

                True :: ((True :: _) as tail) ->
                    SemiCircle :: rec tail

                False :: ((False :: _) as tail) ->
                    SemiCircle :: rec tail

                _ :: ((_ :: _) as tail) ->
                    Line :: rec tail
    in
    ( case l |> List.head of
        Just True ->
            Up

        Just False ->
            Down

        Nothing ->
            Up
    , case l of
        [] ->
            [ Singleton ]

        [ True ] ->
            [ Spiral, End ]

        [ False ] ->
            [ Spiral, End ]

        [ True, False ] ->
            [ Start, LineLoop, End ]

        [ False, True ] ->
            [ Start, LineLoop, End ]

        [ True, False, True ] ->
            [ Start, Loop, End ]

        [ False, True, False ] ->
            [ Start, Loop, End ]

        True :: False :: ((True :: _) as tail) ->
            Start :: Loop :: rec tail

        False :: True :: ((False :: _) as tail) ->
            Start :: Loop :: rec tail

        False :: ((True :: _) as tail) ->
            Start :: Line :: rec tail

        True :: ((False :: _) as tail) ->
            Start :: Line :: rec tail

        _ ->
            Start :: rec l
    )


view :
    { point : Point2d unit coord
    , value : Int
    , size : Int
    , color : String
    , radius : Float
    , strokeWidth : Float
    , direction : Direction2d coord
    }
    -> List (Svg msg)
view { point, value, size, color, radius, strokeWidth, direction } =
    let
        midIndex =
            2 ^ (size - 1)

        ( dir, shapeList ) =
            (if value >= midIndex then
                ((2 ^ size) - 1) - value + midIndex

             else
                value
            )
                |> Binary.fromDecimal
                |> Binary.ensureSize size
                |> Binary.toBooleans
                |> boolListToShapeList

        out =
            shapeList
                |> shapeListToSvg
                    { p = Vector2d.zero
                    , direction = dir
                    , radius = radius
                    }

        turtle : Turtle (List (Svg msg))
        turtle =
            { direction = direction |> Direction2d.toAngle |> Direction2d.fromAngle --Direction2d.positiveX --Direction2d.positiveY --
            , position =
                point
                    |> Point2d.toRecord Quantity.unwrap
                    |> Point2d.unsafe
            , lineFun =
                \{ to, from } ->
                    LineSegment2d.from from to
                        |> Svg.lineSegment2d
                            [ Attributes.stroke color
                            , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                            , Attributes.fill "none"
                            ]
                        |> List.singleton
            , arcFun =
                \{ around, by, from } ->
                    let
                        overshoot =
                            0

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
                    arc
                        |> Svg.arc2d
                            [ Attributes.fill <| "none"
                            , Attributes.stroke <| color
                            , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                            ]
                        |> List.singleton
            }
    in
    turtle
        |> Turtle.rotateRightBy (Angle.radians <| pi / 2)
        |> Turtle.jumpForward
            (case dir of
                Up ->
                    1 * radius

                Down ->
                    5 * radius
            )
        |> Turtle.rotateLeftBy (Angle.radians <| pi / 2)
        --|> Turtle.rotateLeftBy (Angle.radians <| pi)
        |> Turtle.jumpForward (-(out.size |> Vector2d.toRecord Quantity.unwrap |> .x) / 2)
        --|> Turtle.rotateLeftBy (Angle.radians <| pi)
        |> out.drawing
        |> Tuple.second
