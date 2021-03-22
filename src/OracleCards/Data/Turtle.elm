module OracleCards.Data.Turtle exposing (Turtle, forwardBy, rotate)

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Point2d exposing (Point2d)


type alias Turtle out =
    { direction : Direction2d
    , position : Point2d Float ()
    , lineFun : Point2d Float () -> Point2d Float () -> out
    , arcFun : Point2d Float () -> Angle -> Point2d Float () -> out
    }


forwardBy : Float -> Turtle out -> ( Turtle out, List out )
forwardBy amount =
    let
        newPosition =
            turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength amount turtle.direction)
    in
    ( { turtle
        | position = newPosition
      }
    , turtle.position |> lineFun newPosition
    )


rotate : { to : Direction2d, around : Point2d () () } -> Turtle out -> ( Turtle out, List out )
rotate { to, around } turtle =
    let
        angle =
            Direction2d.angleFrom turtle.direction to

        newPosition =
            turtle.position |> Point2d.rotateAround around angle
    in
    ( { turtle
        | direction = to
        , position = newPosition
      }
    , turtle.position |> arcFun around angle |> List.singleton
    )
