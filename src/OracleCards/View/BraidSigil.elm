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
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
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
    0.09



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
    { startAngle : Angle
    , startIndex : Index N
    , lastDistinctIndex : Index N
    , nextIndex : Index N
    , visited : StaticArray N Int
    , isOvercross : Bool
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
                |> Vector2d.fromPixels

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
                        (Vector2d.pixels 1 0
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
                        (Vector2d.pixels 1 0
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
                Pixels.pixels <|
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
                |> Vector2d.fromPixels

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
    { startAngle =
        Direction2d.from
            p2
            end
            |> Maybe.map Direction2d.toAngle
            |> Maybe.withDefault (Angle.radians 0)
    , startIndex = startIndex
    , lastDistinctIndex = nextIndex --lastDistinctIndex
    , nextIndex = startIndex
    , visited = visited
    , isOvercross =
        isOvercross
    }


points : { width : Float, height : Float, radius : Float } -> StaticArray N (Point2d Pixels coord)
points { width, height, radius } =
    let
        rotate r =
            Point2d.pixels (width / 2) (height / 2 - radius)
                |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2))
                    (Angle.radians <| 2 * pi * toFloat r / toFloat (n |> Length.toInt))
    in
    List.range 1 ((n |> Length.toInt) - 1)
        |> List.map rotate
        |> StaticArray.fromList n (rotate 0)


line : { width : Float, height : Float, radius : Float } -> State -> Index N -> ( State, List (Svg msg) )
line options state newNextIndex =
    let
        a0 =
            state.startAngle

        i2 =
            state.nextIndex

        p1 =
            points options |> StaticArray.get state.startIndex

        p2 =
            points options |> StaticArray.get i2

        {--is p1->p2->p3 overcrossed?--}
        isOvercross =
            isOvercrossed
                { i1 = state.lastDistinctIndex
                , i2 = state.nextIndex
                , i3 = newNextIndex
                }

        {--case ( state.startIndex |> isLeftOf state.nextIndex, state.nextIndex |> isLeftOf newNextIndex ) of
                ( True, True ) ->
                    False

                ( False, False ) ->
                    True

                ( True, False ) ->
                    False

                ( False, True ) ->
                    False--}
        vector =
            Vector2d.from p1 p2
                |> Vector2d.normalize
                |> Vector2d.toUnitless
                |> Vector2d.fromPixels

        circle =
            Circle2d.atPoint p2 (Pixels.pixels <| pointSize + lineWidth / 2)

        start =
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

        maybeA1 =
            Direction2d.from p1 start
                |> Maybe.map Direction2d.toAngle

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
                                * (state.visited
                                    |> StaticArray.get i2
                                    |> toFloat
                                    |> (+) 1
                                  )
                            )
                    )

        maybeA2 =
            Direction2d.from p2 end
                |> Maybe.map Direction2d.toAngle

        segment =
            LineSegment2d.from start end
    in
    ( { state
        | startAngle =
            maybeA2
                |> Maybe.withDefault a0
        , startIndex =
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
                |> StaticArray.set i2 (state.visited |> StaticArray.get i2 |> (+) 1)
        , isOvercross =
            isOvercross
      }
    , [ case maybeA1 of
            Just a1 ->
                let
                    diffAngle =
                        (a1 |> Angle.inRadians)
                            - (a0 |> Angle.inRadians)

                    eps =
                        0.001

                    ( startAngle, sweptAngle ) =
                        if state.visited == StaticArray.fromList n 0 [] && isOvercross then
                            ( a1
                            , 0
                                |> Angle.radians
                            )

                        else if diffAngle < 0 - eps then
                            if abs diffAngle > pi + eps then
                                ( a1
                                , abs diffAngle
                                    |> Angle.radians
                                )

                            else if abs diffAngle < pi - eps then
                                ( a0
                                , 2
                                    * pi
                                    - abs diffAngle
                                    |> Angle.radians
                                )

                            else
                                ( a1
                                , abs diffAngle |> Angle.radians
                                )

                        else if diffAngle > 0 + eps then
                            if diffAngle > pi + eps then
                                ( a0
                                , diffAngle |> Angle.radians
                                )

                            else if diffAngle < pi - eps then
                                ( a1
                                , 2
                                    * pi
                                    - diffAngle
                                    |> Angle.radians
                                )

                            else
                                ( a1
                                , diffAngle |> Angle.radians
                                )

                        else
                            ( a0
                            , 0
                                |> Angle.radians
                            )

                    {--if isOvercross then
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
                            )--}
                    arc =
                        Arc2d.with
                            { centerPoint = p1
                            , radius =
                                Pixels.pixels <|
                                    pointSize
                                        + (lineWidth
                                            * (state.visited
                                                |> StaticArray.get state.startIndex
                                                |> toFloat
                                              )
                                          )
                                        - lineWidth
                                        / 2
                            , startAngle = Angle.radians <| (startAngle |> Angle.inRadians) - overshoot
                            , sweptAngle = Angle.radians <| (sweptAngle |> Angle.inRadians) + overshoot * 2
                            }
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

            Nothing ->
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

                    arc =
                        Arc2d.with
                            { centerPoint = p1
                            , radius =
                                Pixels.pixels <|
                                    pointSize
                                        + (lineWidth
                                            * (state.visited
                                                |> StaticArray.get state.startIndex
                                                |> toFloat
                                              )
                                          )
                                        - lineWidth
                                        / 2
                            , startAngle = Angle.radians <| (startAngle |> Angle.inRadians) - overshoot
                            , sweptAngle = Angle.radians <| pi + overshoot * 2
                            }

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
                                    (Vector2d.pixels 1 0
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
                                    (Vector2d.pixels 1 0
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

                    transitionArc =
                        Arc2d.with
                            { centerPoint = centerPoint
                            , radius =
                                Pixels.pixels <|
                                    pointSize
                                        + (lineWidth
                                            * (state.visited
                                                |> StaticArray.get state.startIndex
                                                |> toFloat
                                              )
                                          )
                            , startAngle = Angle.radians <| (transitionStartAngle |> Angle.inRadians) - overshoot
                            , sweptAngle =
                                Angle.radians <| pi + overshoot * 2
                            }
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
                , transitionArc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "black"
                        , Attributes.strokeWidth <| String.fromFloat <| lineWidth
                        ]
                , transitionArc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "white"
                        , Attributes.strokeLinecap "round"
                        , Attributes.strokeWidth <| String.fromFloat <| lineWidth - 2 * strokeWidth
                        ]
                ]
      , [ segment
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
      ]
        |> List.concat
    )
