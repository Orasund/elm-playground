module OracleCards.View.BraidSigil exposing (drawPoints, init, initCircle, line, pointSize, points, strokeWidth)

import Angle exposing (Angle)
import Arc2d
import Circle2d exposing (Circle2d)
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import OracleCards.Data.Alphabet as Alphabet
import OracleCards.Data.Geometry as Geometry
import OracleCards.Data.Turtle as Turtle exposing (Turtle)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Five, Index, OnePlus, TwentyPlus)
import StaticArray.Length as Length exposing (Length)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d


type alias N =
    Alphabet.TwentySix


n : Length N
n =
    Alphabet.twentySix


strokeWidth =
    1


lineWidth =
    4


pointSize =
    lineWidth / 2


overshoot =
    0.05


nextClockwise : { i1 : Index N, i2 : Index N, i3 : Index N } -> Bool
nextClockwise { i1, i2, i3 } =
    let
        n3 =
            i3 |> Index.toInt

        n2 =
            i2 |> Index.toInt

        n1 =
            i1 |> Index.toInt

        l =
            n |> Length.toInt
    in
    (n1 - n2 |> modBy l) < (n3 - n2 |> modBy l)


type alias State =
    { startIndex : Index N
    , lastDistinctIndex : Maybe (Index N)
    , nextIndex : Index N
    , visited : StaticArray N Int
    , isOvercross : Bool
    , turtle : Turtle (List (Svg Never))
    }


initCircle : { width : Float, height : Float, radius : Float } -> Index N -> Index N -> List (Svg msg)
initCircle options startIndex nextIndex =
    let
        p1 =
            points options |> StaticArray.get startIndex

        p2 =
            points options |> StaticArray.get nextIndex

        isNextClockwise =
            nextClockwise
                { i1 = nextIndex --lastDistinctIndex
                , i2 = startIndex
                , i3 = nextIndex
                }

        vector =
            Vector2d.from p1 p2
                |> Vector2d.normalize
                |> Vector2d.toUnitless
                |> Vector2d.unsafe

        amount =
            1

        end =
            p2
                |> Point2d.translateBy
                    (vector
                        |> (if not isNextClockwise then
                                Vector2d.rotateBy (Angle.radians <| -pi / 2)

                            else
                                Vector2d.rotateBy (Angle.radians <| pi / 2)
                           )
                        |> Vector2d.scaleBy
                            (lineWidth
                                * (amount
                                    |> toFloat
                                    |> (+) 1
                                  )
                            )
                    )

        startAngle =
            Direction2d.from
                p2
                end
                |> Maybe.map Direction2d.toAngle
                |> Maybe.withDefault (Angle.radians 0)

        centerposition =
            Point2d.midpoint
                (p1
                    |> Point2d.translateBy
                        (Vector2d.unsafe { x = 1, y = 0 }
                            |> Vector2d.rotateBy startAngle
                            |> Vector2d.scaleBy
                                (pointSize
                                    + (lineWidth
                                        * amount
                                      )
                                    - lineWidth
                                    / 2
                                )
                        )
                )
                (p1
                    |> Point2d.translateBy
                        (Vector2d.unsafe { x = 1, y = 0 }
                            |> Vector2d.rotateBy (startAngle |> Angle.inRadians |> (+) pi |> Angle.radians)
                            |> Vector2d.scaleBy
                                (pointSize
                                    + (lineWidth
                                        * (amount
                                            |> toFloat
                                            |> (+) 1
                                          )
                                      )
                                    - lineWidth
                                    / 2
                                )
                        )
                )

        radius =
            pointSize

        {--+ (lineWidth
                    * amount
                  )--}
        {- lineWidth
           / 2
        -}
    in
    [ Arc2d.with
        { centerPoint = p1
        , radius = Quantity.unsafe <| lineWidth / 2
        , startAngle = Angle.radians -overshoot
        , sweptAngle = Angle.radians <| pi + overshoot * 2
        }
        |> Svg.arc2d
            [ Attributes.fill <| "black"
            ]
    , Arc2d.sweptAround p1
        (Angle.radians <| 2 * pi)
        (p1
            |> Point2d.translateBy
                (Vector2d.unsafe
                    { x =
                        (lineWidth - 2 * strokeWidth)
                            / 2
                    , y = 0
                    }
                )
        )
        |> Svg.arc2d
            [ Attributes.fill <| "white"
            ]
    ]


init :
    { width : Float
    , height : Float
    , radius : Float
    , startIndex : Index N
    , nextIndex : Index N
    , distinctSecond : Index N
    , distinctThird : Index N
    }
    -> ( State, List (Svg Never) )
init { width, height, radius, startIndex, nextIndex, distinctSecond, distinctThird } =
    let
        i2 =
            nextIndex

        options =
            { width = width
            , height = height
            , radius = radius
            }

        p1 =
            points options |> StaticArray.get startIndex

        p2 =
            points options |> StaticArray.get i2

        isNextClockwise =
            nextClockwise
                { i1 = startIndex
                , i2 = distinctSecond
                , i3 = distinctThird
                }

        ( turtle, drawing ) =
            { direction = Direction2d.negativeY
            , position = p1
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
                            Arc2d.sweptAround around
                                (by
                                    |> Angle.inRadians
                                    |> (\r ->
                                            if (r > -pi + overshoot) && (r < pi - overshoot) then
                                                2 * pi

                                            else if r < 0 then
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
                            , Attributes.strokeWidth <| String.fromFloat <| lineWidth
                            ]
                    , arc
                        |> Svg.arc2d
                            [ Attributes.fill <| "none"
                            , Attributes.stroke <| "white"
                            , Attributes.strokeLinecap "round"
                            , Attributes.strokeWidth <| String.fromFloat <| lineWidth - 2 * strokeWidth
                            ]
                    ]
            }
                |> (if isNextClockwise then
                        Turtle.rotateClockwise
                            { to = Direction2d.positiveY
                            , radius = lineWidth / 2
                            }

                    else
                        Turtle.rotateCounterclockwise
                            { to = Direction2d.positiveY
                            , radius = lineWidth / 2
                            }
                   )

        visited =
            StaticArray.fromList n 0 []
                |> StaticArray.set startIndex 1
    in
    ( { startIndex = startIndex
      , lastDistinctIndex = Nothing
      , nextIndex = nextIndex --startIndex
      , visited = visited
      , isOvercross =
            isNextClockwise
      , turtle =
            turtle
      }
    , drawing
    )


points : { width : Float, height : Float, radius : Float } -> StaticArray N (Point2d Float coord)
points { width, height, radius } =
    let
        rotate r =
            Point2d.unsafe { x = width / 2, y = height / 2 - radius }
                |> Point2d.rotateAround (Point2d.unsafe { x = width / 2, y = height / 2 })
                    (Angle.radians <| 2 * pi * toFloat r / toFloat (n |> Length.toInt))
    in
    List.range 1 ((n |> Length.toInt) - 1)
        |> List.map rotate
        |> StaticArray.fromList n (rotate 0)


drawPoints : { width : Float, height : Float, radius : Float } -> List (Svg msg)
drawPoints =
    points
        >> StaticArray.toList
        >> List.map
            (Circle2d.withRadius (Quantity 1)
                >> Svg.circle2d
                    [ Attributes.fill "black"
                    ]
            )



--------------------------------------------------------------------------------
-- CircleAround
--------------------------------------------------------------------------------


circleAround :
    { width : Float, height : Float, radius : Float }
    ->
        { startIndex : Index N
        , visited : StaticArray N Int
        , turtle : Turtle (List (Svg Never))
        , isClockwise : Bool
        }
    -> ( Turtle (List (Svg Never)), List (Svg Never) )
circleAround options { startIndex, turtle, visited, isClockwise } =
    let
        --Direction of the turtle
        d0 =
            turtle.direction

        --current Node
        p1 =
            points options |> StaticArray.get startIndex

        --Nr of visits of current node
        v1 =
            visited
                |> StaticArray.get startIndex

        intersectionPoint =
            turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength
                        (pointSize
                            + (lineWidth * (2 * v1 |> toFloat))
                            - lineWidth
                            / 2
                            |> Quantity
                        )
                        (Direction2d.from turtle.position p1
                            |> Maybe.withDefault Direction2d.positiveX
                        )
                    )

        endPoint =
            turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength
                        ((lineWidth * (1 |> toFloat))
                            |> Quantity
                        )
                        (Direction2d.from p1 turtle.position
                            |> Maybe.withDefault Direction2d.positiveX
                        )
                    )

        centerPoint =
            Point2d.midpoint intersectionPoint endPoint

        rotate =
            if isClockwise then
                Turtle.rotateClockwise

            else
                Turtle.rotateCounterclockwise
    in
    if v1 == -1 then
        turtle
            |> rotate
                { to = turtle.direction
                , radius =
                    turtle.position
                        |> Point2d.distanceFrom p1
                        |> Quantity.unwrap
                }

    else
        turtle
            |> rotate
                { to = turtle.direction |> Direction2d.reverse
                , radius =
                    turtle.position
                        |> Point2d.distanceFrom p1
                        |> Quantity.unwrap
                }
            |> Tuple.mapFirst (\t -> { t | position = intersectionPoint })
            |> Turtle.andThen
                (\t ->
                    t
                        |> rotate
                            { to = turtle.direction
                            , radius =
                                t.position
                                    |> Point2d.distanceFrom centerPoint
                                    |> Quantity.unwrap
                            }
                )
            |> Tuple.mapFirst (\t -> { t | position = endPoint })



--------------------------------------------------------------------------------
-- Line
--------------------------------------------------------------------------------


line : { width : Float, height : Float, radius : Float } -> State -> Index N -> ( State, List (Svg msg) )
line options state newNextIndex =
    {--let
        _ =
            state |> Debug.log "state"
    in--}
    if state.startIndex == state.nextIndex then
        --line2 options state newNextIndex
        let
            isNextClockwise =
                state.lastDistinctIndex
                    |> Maybe.map
                        (\i1 ->
                            if i1 == state.nextIndex then
                                state.isOvercross

                            else
                                nextClockwise
                                    { i1 = i1
                                    , i2 = state.nextIndex
                                    , i3 = newNextIndex
                                    }
                        )
                    |> Maybe.withDefault state.isOvercross

            ( turtle, drawing ) =
                circleAround options
                    { startIndex = state.startIndex
                    , visited = state.visited
                    , turtle = state.turtle
                    , isClockwise = state.isOvercross
                    }
        in
        ( { state
            | lastDistinctIndex =
                if (state.lastDistinctIndex |> Maybe.withDefault state.startIndex) == state.nextIndex then
                    state.lastDistinctIndex

                else
                    Just state.nextIndex
            , nextIndex =
                newNextIndex
            , visited =
                state.visited
                    |> StaticArray.set state.nextIndex (state.visited |> StaticArray.get state.nextIndex |> (+) 1)
            , isOvercross =
                isNextClockwise
            , turtle =
                turtle
          }
        , drawing |> List.map (Svg.map never)
        )

    else
        let
            --current Node
            p1 =
                points options |> StaticArray.get state.startIndex

            --next Node
            p2 =
                points options |> StaticArray.get state.nextIndex

            --Nr of visits of current node
            v1 =
                state.visited
                    |> StaticArray.get state.startIndex

            --Nr of visits of next node
            v2 =
                state.visited
                    |> StaticArray.get state.nextIndex

            --Radius around p1
            r1 =
                Quantity.unsafe (lineWidth * (v1 |> toFloat))

            --Radius around p2
            r2 =
                Quantity (lineWidth * (v2 + 1 |> toFloat))

            isNextClockwise =
                state.lastDistinctIndex
                    |> Maybe.map
                        (\i1 ->
                            if i1 == newNextIndex then
                                not state.isOvercross

                            else
                                nextClockwise
                                    { i1 = i1
                                    , i2 = state.nextIndex
                                    , i3 = newNextIndex
                                    }
                        )
                    |> Maybe.withDefault
                        (if state.startIndex == newNextIndex then
                            not state.isOvercross

                         else
                            state.isOvercross
                        )

            --outer/inner tangents of circles p1 p2
            ( intermediatePosition, endPosition ) =
                if state.isOvercross == isNextClockwise then
                    if v2 + 1 == v1 then
                        let
                            vec =
                                Vector2d.withLength r1
                                    (Direction2d.from p1 p2
                                        |> Maybe.withDefault Direction2d.positiveX
                                        |> (if state.isOvercross then
                                                Direction2d.rotateClockwise

                                            else
                                                Direction2d.rotateCounterclockwise
                                           )
                                    )
                        in
                        ( p1 |> Point2d.translateBy vec, p2 |> Point2d.translateBy vec )

                    else
                        Geometry.outerTangent
                            ( Circle2d.withRadius r1 p1
                            , Circle2d.withRadius r2 p2
                            )
                            isNextClockwise

                else
                    Geometry.innerTangent
                        ( Circle2d.withRadius r1 p1
                        , Circle2d.withRadius r2 p2
                        )
                        isNextClockwise

            --EndDirection of turtle
            endDirection =
                Direction2d.from intermediatePosition endPosition
                    |> Maybe.withDefault Direction2d.positiveX

            rotate =
                if state.isOvercross then
                    Turtle.rotateClockwise

                else
                    Turtle.rotateCounterclockwise

            ( turtle, drawing ) =
                state.turtle
                    |> rotate
                        { to = endDirection
                        , radius =
                            state.turtle.position
                                |> Point2d.distanceFrom p1
                                |> Quantity.unwrap
                        }
                    |> Tuple.mapFirst (\t -> { t | position = intermediatePosition })
                    |> Turtle.andThen
                        (\t ->
                            t
                                |> Turtle.forwardBy
                                    (t.position
                                        |> Point2d.distanceFrom endPosition
                                        |> Quantity.unwrap
                                    )
                        )
        in
        ( { state
            | startIndex =
                state.nextIndex
            , lastDistinctIndex =
                if (state.lastDistinctIndex |> Maybe.withDefault state.startIndex) == state.nextIndex then
                    state.lastDistinctIndex

                else
                    Just state.nextIndex

            {--if state.startIndex == state.nextIndex then
                    --if state.nextIndex == newNextIndex then
                    state.lastDistinctIndex

                else
                    Just state.nextIndex--}
            , nextIndex =
                newNextIndex
            , visited =
                state.visited
                    |> StaticArray.set state.nextIndex (v2 |> (+) 1)
            , isOvercross =
                isNextClockwise
            , turtle =
                { turtle
                    | position = endPosition
                    , direction = endDirection
                }
          }
        , drawing |> List.map (Svg.map never)
        )
