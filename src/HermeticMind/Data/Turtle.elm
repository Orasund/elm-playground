module HermeticMind.Data.Turtle exposing (Turtle, andThen, forwardBy, rotateClockwise, rotateClockwiseBy, rotateClockwiseTo, rotateCounterclockwise, rotateCounterclockwiseBy, rotateCounterclockwiseTo)

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Vector2d


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


rotateCounterclockwiseBy : { angle : Angle, radius : Float } -> Turtle out -> ( Turtle out, out )
rotateCounterclockwiseBy { angle, radius } turtle =
    rotateCounterclockwiseTo
        { direction = turtle.direction |> Direction2d.rotateBy (angle |> Angle.inRadians |> (-) (2 * pi) |> Angle.radians)
        , radius = radius
        }
        turtle


rotateCounterclockwiseTo : { direction : Direction2d (), radius : Float } -> Turtle out -> ( Turtle out, out )
rotateCounterclockwiseTo { direction, radius } =
    rotateCounterclockwise { to = direction, radius = radius }


rotateCounterclockwise : { to : Direction2d (), radius : Float } -> Turtle out -> ( Turtle out, out )
rotateCounterclockwise { to, radius } turtle =
    let
        angle =
            Direction2d.angleFrom turtle.direction to
                |> Angle.inRadians
                |> (\r ->
                        if
                            to
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
        | direction = to
        , position = newPosition
      }
    , turtle.arcFun { around = around, by = angle, from = turtle.position }
    )


rotateClockwiseBy : { angle : Angle, radius : Float } -> Turtle out -> ( Turtle out, out )
rotateClockwiseBy { angle, radius } turtle =
    rotateClockwiseTo
        { direction = turtle.direction |> Direction2d.rotateBy angle
        , radius = radius
        }
        turtle


rotateClockwiseTo : { direction : Direction2d (), radius : Float } -> Turtle out -> ( Turtle out, out )
rotateClockwiseTo { direction, radius } =
    rotateClockwise { to = direction, radius = radius }


rotateClockwise : { to : Direction2d (), radius : Float } -> Turtle out -> ( Turtle out, out )
rotateClockwise { to, radius } turtle =
    let
        angle =
            Direction2d.angleFrom turtle.direction to
                |> Angle.inRadians
                |> (\r ->
                        if
                            to
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
        | direction = to
        , position = newPosition
      }
    , turtle.arcFun { around = around, by = angle, from = turtle.position }
    )


andThen : (Turtle (List out) -> ( Turtle (List out), List out )) -> ( Turtle (List out), List out ) -> ( Turtle (List out), List out )
andThen fun ( turtle, out ) =
    let
        ( newTurtle, head ) =
            fun turtle
    in
    ( newTurtle, head ++ out )
