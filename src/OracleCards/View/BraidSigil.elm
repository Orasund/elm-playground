module OracleCards.View.BraidSigil exposing (init, initCircle, line, pointSize, strokeWidth)

import Angle exposing (Angle)
import Arc2d
import Circle2d
import Direction2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import OracleCards.Data.Alphabet as Alphabet
import OracleCards.Data.Turtle as Turtle exposing (Turtle)
import OracleCards.View.Path as Path
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



{--isLeftOf : Index N -> Index N -> Bool
isLeftOf i2 i1 =
    ((i2 |> Index.toInt) - (i1 |> Index.toInt) |> modBy (n |> Index.last |> Index.toInt |> (+) 1))
        < ((n |> Index.last |> Index.toInt |> (+) 1)
            // 2
          )--}


isOvercrossed : { i1 : Index N, i2 : Index N, i3 : Index N } -> Bool
isOvercrossed { i1, i2, i3 } =
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
    , lastDistinctIndex : Index N
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

        isOvercross =
            isOvercrossed
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
                        |> (if not isOvercross then
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
        circle =
            Circle2d.atPoint centerposition <|
                Quantity <|
                    radius
    in
    [ circle
        |> Svg.circle2d
            [ Attributes.fill <| "none"
            , Attributes.stroke <| "black"
            , Attributes.strokeWidth <| String.fromFloat <| lineWidth
            ]
    , circle
        |> Svg.circle2d
            [ Attributes.fill <| "none"
            , Attributes.stroke <| "white"
            , Attributes.strokeWidth <| String.fromFloat <| lineWidth - 2 * strokeWidth
            ]
    ]


init : { width : Float, height : Float, radius : Float } -> Index N -> Index N -> State
init options startIndex nextIndex =
    let
        i2 =
            nextIndex

        p1 =
            points options |> StaticArray.get startIndex

        p2 =
            points options |> StaticArray.get i2

        isOvercross =
            isOvercrossed
                { i1 = nextIndex --lastDistinctIndex
                , i2 = startIndex
                , i3 = nextIndex
                }

        vector =
            Vector2d.from p1 p2
                |> Vector2d.normalize
                |> Vector2d.toUnitless
                |> Vector2d.unsafe

        visited =
            StaticArray.fromList n 0 []

        end =
            p2
                |> Point2d.translateBy
                    (vector
                        |> (if isOvercross then
                                Vector2d.rotateBy (Angle.radians <| -pi / 2)

                            else
                                Vector2d.rotateBy (Angle.radians <| pi / 2)
                           )
                        |> Vector2d.scaleBy
                            (lineWidth
                                * (visited
                                    |> StaticArray.get i2
                                    |> toFloat
                                    |> (+) 1
                                  )
                            )
                    )
    in
    { startIndex = startIndex
    , lastDistinctIndex = nextIndex --lastDistinctIndex
    , nextIndex = startIndex
    , visited = visited
    , isOvercross =
        isOvercross
    , turtle =
        { direction =
            Direction2d.from p2 end
                |> Maybe.withDefault Direction2d.positiveX
        , position = p1
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
    }


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


line : { width : Float, height : Float, radius : Float } -> State -> Index N -> ( State, List (Svg msg) )
line options state newNextIndex =
    let
        --Position of the turtle
        p0 =
            state.turtle.position

        --Direction of the turtle
        a0 =
            state.turtle.direction |> Direction2d.toAngle

        p1 =
            points options |> StaticArray.get state.startIndex

        p2 =
            points options |> StaticArray.get state.nextIndex

        {--is p1->p2->p3 overcrossed?--}
        isOvercross =
            isOvercrossed
                { i1 = state.lastDistinctIndex
                , i2 = state.nextIndex
                , i3 = newNextIndex
                }

        vector =
            Vector2d.from p1 p2
                |> Vector2d.normalize
                |> Vector2d.toUnitless
                |> Vector2d.unsafe

        --Position after Rotation
        startPosition =
            p1
                |> Point2d.translateBy
                    (vector
                        |> Vector2d.scaleBy
                            (lineWidth
                                * (state.visited
                                    |> StaticArray.get state.startIndex
                                    |> toFloat
                                  )
                            )
                        |> (if state.isOvercross then
                                Vector2d.rotateBy (Angle.radians <| -pi / 2)

                            else
                                Vector2d.rotateBy (Angle.radians <| pi / 2)
                           )
                    )

        --Endposition of turtle
        endPosition =
            p2
                |> Point2d.translateBy
                    (vector
                        |> (if isOvercross then
                                Vector2d.rotateBy (Angle.radians <| -pi / 2)

                            else
                                Vector2d.rotateBy (Angle.radians <| pi / 2)
                           )
                        |> Vector2d.scaleBy
                            (lineWidth
                                * (state.visited
                                    |> StaticArray.get state.nextIndex
                                    |> toFloat
                                    |> (+) 1
                                  )
                            )
                    )

        maybeA1 =
            Direction2d.from p1 startPosition

        maybeA2 =
            Direction2d.from p2 endPosition

        rotate =
            if isOvercross then
                Turtle.rotateClockwise

            else
                Turtle.rotateCounterclockwise

        ( turtle, drawing ) =
            if p1 == p2 then
                let
                    a1 =
                        a0
                            |> Angle.inRadians
                            |> (+) pi
                            |> Angle.radians

                    ( startAngle, sweptAngle ) =
                        if
                            (a1 |> Angle.inRadians)
                                < (a0 |> Angle.inRadians)
                        then
                            ( a0
                            , (a1 |> Angle.inRadians)
                                - (a0 |> Angle.inRadians)
                                |> Angle.radians
                            )

                        else
                            ( a1
                            , (2 * pi)
                                - (a1 |> Angle.inRadians)
                                + (a0 |> Angle.inRadians)
                                |> Angle.radians
                            )

                    ( transitionStartAngle, transitionSweptAngle ) =
                        if
                            ((startAngle |> Angle.inRadians |> (+) pi |> Angle.radians) |> Angle.inRadians)
                                < (startAngle |> Angle.inRadians)
                        then
                            ( startAngle
                            , ((startAngle |> Angle.inRadians |> (+) pi |> Angle.radians) |> Angle.inRadians)
                                - (startAngle |> Angle.inRadians)
                                |> Angle.radians
                            )

                        else
                            ( startAngle |> Angle.inRadians |> (+) pi |> Angle.radians
                            , (2 * pi)
                                - ((startAngle |> Angle.inRadians |> (+) pi |> Angle.radians) |> Angle.inRadians)
                                + (startAngle |> Angle.inRadians)
                                |> Angle.radians
                            )

                    centerPoint =
                        Point2d.midpoint
                            (p1
                                |> Point2d.translateBy
                                    (Vector2d.unsafe { x = 1, y = 0 }
                                        |> Vector2d.rotateBy startAngle
                                        |> Vector2d.scaleBy
                                            (pointSize
                                                + (lineWidth
                                                    * (state.visited
                                                        |> StaticArray.get state.startIndex
                                                        |> toFloat
                                                      )
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
                                                    * (state.visited
                                                        |> StaticArray.get state.startIndex
                                                        |> toFloat
                                                        |> (+) 1
                                                      )
                                                  )
                                                - lineWidth
                                                / 2
                                            )
                                    )
                            )
                in
                state.turtle
                    |> rotate
                        { to = state.turtle.direction |> Direction2d.rotateBy (Angle.radians pi)
                        , radius =
                            state.turtle.position
                                |> Point2d.distanceFrom centerPoint
                                |> Quantity.unwrap
                        }
                    |> Turtle.andThen
                        (rotate
                            { to = state.turtle.direction |> Direction2d.rotateBy (Angle.radians pi)
                            , radius =
                                state.turtle.position
                                    |> Point2d.distanceFrom p1
                                    |> Quantity.unwrap
                            }
                        )

            else
                state.turtle
                    |> rotate
                        { to = vector |> Vector2d.direction |> Maybe.withDefault Direction2d.positiveX
                        , radius =
                            state.turtle.position
                                |> Point2d.distanceFrom p1
                                |> Quantity.unwrap
                        }
                    |> Turtle.andThen
                        (Turtle.forwardBy (vector |> Vector2d.length |> Quantity.unwrap))
    in
    Path.line
        { width = options.width
        , height = options.height
        , radius = options.radius
        , startingPosition = startPosition
        , startingDireciton = maybeA1
        , movingAround = p1
        , endingPosition = endPosition
        , endingDirection = maybeA2
        , lineWidth = lineWidth
        , pointSize = pointSize
        , strokeWidth = strokeWidth
        }
        state
        newNextIndex



{--( { state
        | startIndex =
            state.nextIndex
        , lastDistinctIndex =
            if state.nextIndex == newNextIndex then
                state.lastDistinctIndex

            else
                state.nextIndex
        , nextIndex =
            newNextIndex
        , visited =
            state.visited
                |> StaticArray.set state.nextIndex (state.visited |> StaticArray.get state.nextIndex |> (+) 1)
        , isOvercross =
            isOvercross
        , turtle = turtle
      }
    , drawing |> List.map (Svg.map never)
    )--}
