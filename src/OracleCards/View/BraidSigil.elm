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


overshoot =
    0.05


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


circleAround : { width : Float, height : Float, radius : Float } -> State -> Bool -> ( Turtle (List (Svg Never)), List (Svg Never) )
circleAround options state nextIsOvercross =
    let
        --Direction of the turtle
        d0 =
            state.turtle.direction

        --current Node
        p1 =
            points options |> StaticArray.get state.startIndex

        --Nr of visits of current node
        v1 =
            state.visited
                |> StaticArray.get state.startIndex

        rotate =
            if not state.isOvercross then
                Turtle.rotateCounterclockwise

            else
                Turtle.rotateClockwise

        a1 =
            d0
                |> Direction2d.reverse

        intersectionPoint =
            state.turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength
                        (pointSize
                            + (lineWidth * (v1 + 1 |> toFloat))
                            - lineWidth
                            / 2
                            |> Quantity
                        )
                        (Direction2d.from state.turtle.position p1
                            |> Maybe.withDefault Direction2d.positiveX
                        )
                    )

        endPoint =
            state.turtle.position
                |> Point2d.translateBy
                    (Vector2d.withLength
                        ((lineWidth * (1 |> toFloat))
                            |> Quantity
                        )
                        (Direction2d.from p1 state.turtle.position
                            |> Maybe.withDefault Direction2d.positiveX
                        )
                    )

        centerPoint =
            Point2d.midpoint intersectionPoint endPoint
    in
    state.turtle
        |> rotate
            { to = state.turtle.direction |> Direction2d.reverse
            , radius =
                state.turtle.position
                    |> Point2d.distanceFrom p1
                    |> Quantity.unwrap
            }
        |> Tuple.mapFirst (\t -> { t | position = intersectionPoint })
        |> Turtle.andThen
            (\t ->
                t
                    |> rotate
                        { to = state.turtle.direction
                        , radius =
                            t.position
                                |> Point2d.distanceFrom centerPoint
                                |> Quantity.unwrap
                        }
            )
        |> Tuple.mapFirst (\t -> { t | position = endPoint })


{-| <https://mathworld.wolfram.com/Circle-CircleIntersection.html#:~:text=The%20intersections%20of%20two%20circles,known%20as%20the%20radical%20center>.
-}
intersection : ( Circle2d Float (), Circle2d Float () ) -> ( Point2d Float (), Point2d Float () )
intersection ( c1, c2 ) =
    let
        p1 =
            c1 |> Circle2d.centerPoint

        p2 =
            c2 |> Circle2d.centerPoint

        d12 =
            Point2d.distanceFrom p1 p2

        dir12 =
            Direction2d.from p1 p2
                |> Maybe.withDefault Direction2d.positiveX

        r1 =
            c1 |> Circle2d.radius

        r2 =
            c2 |> Circle2d.radius

        d1 =
            ((d12 |> Quantity.unwrap)
                * (d12 |> Quantity.unwrap)
                - ((r2 |> Quantity.unwrap) * (r2 |> Quantity.unwrap))
                + ((r1 |> Quantity.unwrap) * (r1 |> Quantity.unwrap))
            )
                / ((d12 |> Quantity.unwrap) * 2)
                |> Quantity.unsafe

        h =
            ((r1 |> Quantity.unwrap) * (r1 |> Quantity.unwrap) - (d1 |> Quantity.unwrap) * (d1 |> Quantity.unwrap))
                |> sqrt
                |> Quantity.unsafe

        x1 =
            p1
                |> Point2d.translateBy (Vector2d.withLength d1 dir12)
                |> Point2d.translateBy (Vector2d.withLength h (dir12 |> Direction2d.rotateClockwise))

        x2 =
            p1
                |> Point2d.translateBy (Vector2d.withLength d1 dir12)
                |> Point2d.translateBy (Vector2d.withLength h (dir12 |> Direction2d.rotateCounterclockwise))
    in
    ( x1, x2 )


{-|

    1. Set M1 = center of bigger circle, M2 = smaller one
    2. Construct Circle k1 with center M1 and radius (r1 - r2)
    3. Set M3 = center of M1, M2
    4. Set x = crosspoint of circles M1 with radius r1+r2/2 and M3 with radius r2
    5. Set d = direction from M1 to x
    6. return points on circle of M1 M2 with direction d

    https://mmf.univie.ac.at/fileadmin/user_upload/p_mathematikmachtfreunde/Materialien/KB-Gemeinsame_Tangenten_zweier_Kreise-Ausarbeitung.pdf

-}
outerTangent : ( Circle2d Float (), Circle2d Float () ) -> Bool -> ( Point2d Float (), Point2d Float () )
outerTangent ( c1, c2 ) isNextClockwise =
    if c1 |> Circle2d.radius |> Quantity.lessThan (c2 |> Circle2d.radius) then
        outerTangent ( c2, c1 ) (not isNextClockwise)
            |> (\( p2, p1 ) -> ( p1, p2 ))

    else
        let
            p1 =
                c1 |> Circle2d.centerPoint

            p2 =
                c2 |> Circle2d.centerPoint

            r1 =
                c1 |> Circle2d.radius

            r2 =
                c2 |> Circle2d.radius

            c3 =
                Circle2d.withRadius ((r1 |> Quantity.unwrap) - (r2 |> Quantity.unwrap) |> Quantity.unsafe) p1

            d12 =
                Point2d.distanceFrom p1 p2

            c4 =
                --checked d12 /2 is correct
                Circle2d.withRadius (d12 |> Quantity.divideBy 2)
                    (Point2d.midpoint p1 p2)

            ( x1, x2 ) =
                intersection ( c3, c4 )

            d =
                if isNextClockwise then
                    Direction2d.from p1 x1
                        |> Maybe.withDefault Direction2d.positiveX

                else
                    Direction2d.from p1 x2
                        |> Maybe.withDefault Direction2d.positiveX
        in
        ( p1 |> Point2d.translateBy (Vector2d.withLength r1 d)
        , p2 |> Point2d.translateBy (Vector2d.withLength r2 d)
        )


{-|

    1. Set M1 = center of bigger circle, M2 = smaller one
    2. Construct Circle k3 with center M1 and radius (r1 + r2)
    3. Set M3 = center of M1, M2
    4. Set x1 = crosspoint of circles k3  and M3 with radius M1->M2/2
    5. Set d = direction from M1 to x
    6. return points on circle of M1 with direction d and M2 with direction -d

    https://mmf.univie.ac.at/fileadmin/user_upload/p_mathematikmachtfreunde/Materialien/KB-Gemeinsame_Tangenten_zweier_Kreise-Ausarbeitung.pdf

-}
innerTangent : ( Circle2d Float (), Circle2d Float () ) -> Bool -> ( Point2d Float (), Point2d Float () )
innerTangent ( c1, c2 ) isNextClockwise =
    if c1 |> Circle2d.radius |> Quantity.lessThan (c2 |> Circle2d.radius) then
        innerTangent ( c2, c1 ) isNextClockwise
            --(not isNextClockwise)
            |> (\( p2, p1 ) -> ( p1, p2 ))

    else
        let
            p1 =
                c1 |> Circle2d.centerPoint

            p2 =
                c2 |> Circle2d.centerPoint

            r1 =
                c1 |> Circle2d.radius

            r2 =
                c2 |> Circle2d.radius

            c3 =
                Circle2d.withRadius (r1 |> Quantity.plus r2) p1

            d12 =
                Point2d.distanceFrom p1 p2

            c4 =
                Circle2d.withRadius (d12 |> Quantity.divideBy 2)
                    (Point2d.midpoint p1 p2)

            ( x1, x2 ) =
                intersection ( c3, c4 )

            d =
                if isNextClockwise then
                    Direction2d.from p1 x2
                        |> Maybe.withDefault Direction2d.positiveX

                else
                    Direction2d.from p1 x1
                        |> Maybe.withDefault Direction2d.positiveX
        in
        ( p1 |> Point2d.translateBy (Vector2d.withLength r1 d)
        , p2 |> Point2d.translateBy (Vector2d.withLength r2 (d |> Direction2d.reverse))
        )


line : { width : Float, height : Float, radius : Float } -> State -> Index N -> ( State, List (Svg msg) )
line options state newNextIndex =
    if state.startIndex == state.nextIndex then
        --line2 options state newNextIndex
        let
            nextIsOvercross =
                isOvercrossed
                    { i1 = state.lastDistinctIndex
                    , i2 = state.nextIndex
                    , i3 = newNextIndex
                    }

            ( turtle, drawing ) =
                circleAround options state nextIsOvercross
        in
        ( { state
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
                nextIsOvercross
            , turtle =
                turtle

            {--{ turtle
                | position = endPosition
            }--}
          }
        , drawing |> List.map (Svg.map never)
        )
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
          }
        , []
        )--}

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
                if state.lastDistinctIndex == newNextIndex then
                    not state.isOvercross

                else
                    nextClockwise
                        { i1 = state.lastDistinctIndex
                        , i2 = state.nextIndex
                        , i3 = newNextIndex
                        }

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
                        outerTangent
                            ( Circle2d.withRadius r1 p1
                            , Circle2d.withRadius r2 p2
                            )
                            isNextClockwise

                else
                    innerTangent
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
                if state.nextIndex == newNextIndex then
                    state.lastDistinctIndex

                else
                    state.nextIndex
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


line2 : { width : Float, height : Float, radius : Float } -> State -> Index N -> ( State, List (Svg msg) )
line2 options state newNextIndex =
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
                Turtle.rotateCounterclockwise

            else
                Turtle.rotateClockwise

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
                                |> Point2d.distanceFrom p1
                                --centerPoint
                                |> Quantity.unwrap
                        }
                    |> Turtle.andThen
                        (\t ->
                            t
                                |> rotate
                                    { to = t.direction |> Direction2d.rotateBy (Angle.radians pi)
                                    , radius =
                                        t.position
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
                        (\t ->
                            t
                                |> Turtle.forwardBy
                                    (t.position
                                        |> Point2d.distanceFrom endPosition
                                        |> Quantity.unwrap
                                    )
                        )
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
