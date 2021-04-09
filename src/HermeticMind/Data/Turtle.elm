module HermeticMind.Data.Turtle exposing (Turtle, andThen, arcLeftBy, arcLeftTo, arcRightBy, arcRightTo, forwardBy, init, jumpBy, jumpForward, jumpTo, map, rotateLeftBy, rotateRightBy, rotateTo)

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Vector2d exposing (Vector2d)


type alias Turtle out =
    { direction : Direction2d ()
    , position : Point2d Float ()
    , lineFun : { to : Point2d Float (), from : Point2d Float () } -> out
    , arcFun : { around : Point2d Float (), by : Angle, from : Point2d Float () } -> out
    }


forwardBy : Float -> Turtle out -> ( Turtle out, out )
forwardBy amount turtle =
    let
        newPosition =
            turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength (Quantity amount) turtle.direction)
    in
    ( { turtle
        | position = newPosition
      }
    , turtle.lineFun { to = newPosition, from = turtle.position }
    )


arcLeftBy : { angle : Angle, radius : Float } -> Turtle out -> ( Turtle out, out )
arcLeftBy { angle, radius } turtle =
    arcLeftTo
        { direction = turtle.direction |> Direction2d.rotateBy (angle |> Angle.inRadians |> (-) (2 * pi) |> Angle.radians)
        , radius = radius
        }
        turtle


arcLeftTo : { direction : Direction2d (), radius : Float } -> Turtle out -> ( Turtle out, out )
arcLeftTo { direction, radius } turtle =
    let
        angle =
            Direction2d.angleFrom turtle.direction direction
                |> Angle.inRadians
                |> (\r ->
                        if
                            direction
                                |> Direction2d.equalWithin
                                    (Angle.radians 0.001)
                                    turtle.direction
                        then
                            2 * pi

                        else if r < 0 then
                            r

                        else
                            r - 2 * pi
                   )
                |> Angle.radians

        around =
            turtle.position
                |> Point2d.translateBy
                    (turtle.direction
                        |> Direction2d.rotateClockwise
                        |> Vector2d.withLength (Quantity radius)
                    )

        newPosition =
            turtle.position |> Point2d.rotateAround around angle
    in
    ( { turtle
        | direction = direction
        , position = newPosition
      }
    , turtle.arcFun { around = around, by = angle, from = turtle.position }
    )


arcRightBy : { angle : Angle, radius : Float } -> Turtle out -> ( Turtle out, out )
arcRightBy { angle, radius } turtle =
    arcRightTo
        { direction = turtle.direction |> Direction2d.rotateBy angle
        , radius = radius
        }
        turtle


arcRightTo : { direction : Direction2d (), radius : Float } -> Turtle out -> ( Turtle out, out )
arcRightTo { direction, radius } turtle =
    let
        angle =
            Direction2d.angleFrom turtle.direction direction
                |> Angle.inRadians
                |> (\r ->
                        if
                            direction
                                |> Direction2d.equalWithin
                                    (Angle.radians 0.001)
                                    turtle.direction
                        then
                            2 * pi

                        else if r < 0 then
                            r + 2 * pi

                        else
                            r
                   )
                |> Angle.radians

        around =
            turtle.position
                |> Point2d.translateBy
                    (turtle.direction
                        |> Direction2d.rotateCounterclockwise
                        |> Vector2d.withLength (Quantity radius)
                    )

        newPosition =
            turtle.position |> Point2d.rotateAround around angle
    in
    ( { turtle
        | direction = direction
        , position = newPosition
      }
    , turtle.arcFun { around = around, by = angle, from = turtle.position }
    )


init : List out -> Turtle (List out) -> ( Turtle (List out), List out )
init out turtle =
    ( turtle, out )


jumpTo : Point2d Float () -> Turtle out -> Turtle out
jumpTo position turtle =
    { turtle | position = position }


jumpBy : Vector2d Float () -> Turtle out -> Turtle out
jumpBy vector turtle =
    { turtle | position = turtle.position |> Point2d.translateBy vector }


jumpForward : Float -> Turtle out -> Turtle out
jumpForward amount turtle =
    { turtle
        | position =
            turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength (Quantity amount) turtle.direction)
    }


rotateTo : Direction2d () -> Turtle out -> Turtle out
rotateTo direction turtle =
    { turtle | direction = direction }


{-| Rotate the turtle clockwise by a given angle.
-}
rotateRightBy : Angle -> Turtle out -> Turtle out
rotateRightBy angle turtle =
    { turtle | direction = turtle.direction |> Direction2d.rotateBy angle }


{-| Rotate the turtle counterclockwise by a given angle.
-}
rotateLeftBy : Angle -> Turtle out -> Turtle out
rotateLeftBy angle turtle =
    { turtle
        | direction =
            turtle.direction
                |> Direction2d.rotateBy
                    (angle
                        |> Angle.inRadians
                        |> (-) (2 * pi)
                        |> Angle.radians
                    )
    }


andThen : (Turtle (List out) -> ( Turtle (List out), List out )) -> ( Turtle (List out), List out ) -> ( Turtle (List out), List out )
andThen fun ( turtle, out ) =
    let
        ( newTurtle, head ) =
            fun turtle
    in
    ( newTurtle, head ++ out )


map : (Turtle out -> Turtle out) -> ( Turtle out, out ) -> ( Turtle out, out )
map =
    Tuple.mapFirst
