module OracleCards.BraidSigil exposing (..)

import Angle exposing (Angle)
import Arc2d
import Array
import Binary
import Circle2d
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import OracleCards.Sigil as Sigil
import OracleCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Five, Index, OnePlus)
import StaticArray.Length as Length exposing (Length)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


type alias N =
    OnePlus Five


n : Length N
n =
    Length.five |> Length.plus1



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


init : Index N -> Index N -> State
init startIndex nextIndex =
    let
        i2 =
            nextIndex

        p1 =
            points |> StaticArray.get startIndex

        p2 =
            points |> StaticArray.get i2

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


size =
    View.relative <| 50


strokeWidth =
    View.relative <| 0.1


lineWidth =
    View.relative <| 1


radius =
    View.relative <| 10


pointSize =
    lineWidth / 2


points : StaticArray N (Point2d Pixels coord)
points =
    let
        rotate r =
            Point2d.pixels (View.width / 2) (View.height / 2 - radius)
                |> Point2d.rotateAround (Point2d.pixels (View.width / 2) (View.height / 2))
                    (Angle.radians <| 2 * pi * toFloat r / toFloat (n |> Length.toInt))
    in
    List.range 1 ((n |> Length.toInt) - 1)
        |> List.map rotate
        |> StaticArray.fromList (Length.five |> Length.plus1) (rotate 0)


line : State -> Index N -> ( State, List (Svg msg) )
line state newNextIndex =
    let
        a0 =
            state.startAngle

        i2 =
            state.nextIndex

        p1 =
            points |> StaticArray.get state.startIndex

        p2 =
            points |> StaticArray.get i2

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
                            , startAngle = startAngle
                            , sweptAngle = sweptAngle
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
                            , startAngle = startAngle
                            , sweptAngle = Angle.radians <| pi --sweptAngle
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
                            , startAngle = transitionStartAngle
                            , sweptAngle = Angle.radians <| pi --transitionSweptAngle
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


main : Html msg
main =
    let
        paths =
            [ [ 0, 4, 2, 0, 3, 5, 1, 3 ]
            , [ 0, 0, 0, 0, 0, 0 ]
            , [ 0, 1, 2, 3, 4, 5 ]
            , [ 0, 3, 0, 3, 0, 3 ]
            , [ 0, 1, 2, 0, 1, 2 ]
            , [ 0, 2, 4, 0, 2, 4 ]
            , [ 0, 4, 2, 0, 4, 2 ]
            , [ 0, 2, 4, 3 ]
            , [ 0, 1, 3, 5, 2, 1, 4, 5, 6, 3, 3, 4, 1, 2 ]
            ]
    in
    paths
        |> List.map
            (\list ->
                [ case list |> List.map (Index.fromModBy n) of
                    head :: nextIndex :: tail ->
                        head
                            :: nextIndex
                            :: tail
                            |> List.foldl
                                (\i2 ( state, out ) ->
                                    let
                                        ( newState, newOut ) =
                                            line state i2
                                    in
                                    ( newState
                                    , newOut :: out
                                    )
                                )
                                ( init head nextIndex, [] )
                            |> (\( state, out ) ->
                                    let
                                        ( newState, newOut ) =
                                            line state head
                                    in
                                    ( newState
                                    , newOut :: out
                                    )
                               )
                            |> (\( state, out ) ->
                                    (line
                                        { state
                                            | visited = state.visited |> StaticArray.set head -1
                                        }
                                        head
                                        |> Tuple.second
                                    )
                                        :: out
                               )
                            |> List.concat

                    _ ->
                        []
                ]
                    |> List.concat
                    |> Svg.svg
                        [ Attributes.width <| (String.fromFloat <| View.zoom * size) ++ "px"
                        , Attributes.height <| (String.fromFloat <| View.zoom * size) ++ "px"
                        , Attributes.version <| "1.1"
                        , Attributes.viewBox <|
                            "0 0 "
                                ++ String.fromFloat size
                                ++ " "
                                ++ String.fromFloat size
                        ]
            )
        |> Html.div []
