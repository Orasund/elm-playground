module OracleCards.View.Path exposing (line)

import Angle exposing (Angle)
import Arc2d
import Circle2d
import Direction2d exposing (Direction2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import OracleCards.Data.Alphabet as Alphabet
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


overshoot =
    0.09


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


line :
    { width : Float
    , height : Float
    , radius : Float
    , startingPosition : Point2d Float ()
    , startingDireciton : Maybe (Direction2d ())
    , movingAround : Point2d Float ()
    , endingPosition : Point2d Float ()
    , endingDirection : Maybe (Direction2d ())
    , pointSize : Float
    , lineWidth : Float
    , strokeWidth : Float
    }
    ->
        { startIndex : Index N
        , lastDistinctIndex : Index N
        , nextIndex : Index N
        , visited : StaticArray N Int
        , isOvercross : Bool
        , turtle : Turtle (List (Svg Never))
        }
    -> Index N
    ->
        ( { startIndex : Index N
          , lastDistinctIndex : Index N
          , nextIndex : Index N
          , visited : StaticArray N Int
          , isOvercross : Bool
          , turtle : Turtle (List (Svg Never))
          }
        , List (Svg msg)
        )
line options state newNextIndex =
    let
        a0 =
            state.turtle.direction |> Direction2d.toAngle

        i2 =
            state.nextIndex

        p1 =
            options.movingAround

        {--is p1->p2->p3 overcrossed?--}
        isOvercross =
            isOvercrossed
                { i1 = state.lastDistinctIndex
                , i2 = state.nextIndex
                , i3 = newNextIndex
                }

        start =
            options.startingPosition

        maybeA1 =
            options.startingDireciton |> Maybe.map Direction2d.toAngle

        end =
            options.endingPosition

        maybeA2 =
            options.endingDirection

        segment =
            LineSegment2d.from start end
    in
    ( case maybeA2 of
        Just a2 ->
            let
                ( newTurtle, arc ) =
                    state.turtle
                        |> (if isOvercross then
                                Turtle.rotateClockwise

                            else
                                Turtle.rotateCounterclockwise
                           )
                            { to = a2
                            , radius =
                                start
                                    |> Point2d.distanceFrom p1
                                    |> Quantity.unwrap
                            }
            in
            { state
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
                        |> StaticArray.set i2 (state.visited |> StaticArray.get i2 |> (+) 1)
                , isOvercross =
                    isOvercross
                , turtle =
                    newTurtle

                {--{ newTurtle
                        | position = end
                    }--}
            }

        Nothing ->
            { state
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

                    arc =
                        Arc2d.with
                            { centerPoint = p1
                            , radius =
                                Quantity <|
                                    options.pointSize
                                        + (options.lineWidth
                                            * (state.visited
                                                |> StaticArray.get state.startIndex
                                                |> toFloat
                                              )
                                          )
                                        - options.lineWidth
                                        / 2
                            , startAngle = Angle.radians <| (startAngle |> Angle.inRadians) - overshoot
                            , sweptAngle = Angle.radians <| (sweptAngle |> Angle.inRadians) + overshoot * 2
                            }
                in
                [ arc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "black"
                        , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth
                        ]
                , arc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "white"
                        , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth - 2 * options.strokeWidth
                        ]
                ]

            Nothing ->
                --Start of the drawing
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
                                Quantity <|
                                    options.pointSize
                                        + (options.lineWidth
                                            * (state.visited
                                                |> StaticArray.get state.startIndex
                                                |> toFloat
                                              )
                                          )
                                        - options.lineWidth
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
                                    (Vector2d.unsafe { x = 1, y = 0 }
                                        |> Vector2d.rotateBy startAngle
                                        |> Vector2d.scaleBy
                                            (options.pointSize
                                                + (options.lineWidth
                                                    * (state.visited
                                                        |> StaticArray.get state.startIndex
                                                        |> toFloat
                                                      )
                                                  )
                                                - options.lineWidth
                                                / 2
                                            )
                                    )
                            )
                            (p1
                                |> Point2d.translateBy
                                    (Vector2d.unsafe { x = 1, y = 0 }
                                        |> Vector2d.rotateBy (startAngle |> Angle.inRadians |> (+) pi |> Angle.radians)
                                        |> Vector2d.scaleBy
                                            (options.pointSize
                                                + (options.lineWidth
                                                    * (state.visited
                                                        |> StaticArray.get state.startIndex
                                                        |> toFloat
                                                        |> (+) 1
                                                      )
                                                  )
                                                - options.lineWidth
                                                / 2
                                            )
                                    )
                            )

                    transitionArc =
                        Arc2d.with
                            { centerPoint = centerPoint
                            , radius =
                                Quantity <|
                                    options.pointSize
                                        + (options.lineWidth
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
                        , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth
                        ]
                , arc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "white"
                        , Attributes.strokeLinecap "round"
                        , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth - 2 * options.strokeWidth
                        ]
                , transitionArc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "black"
                        , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth
                        ]
                , transitionArc
                    |> Svg.arc2d
                        [ Attributes.fill <| "none"
                        , Attributes.stroke <| "white"
                        , Attributes.strokeLinecap "round"
                        , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth - 2 * options.strokeWidth
                        ]
                ]
      , [ segment
            |> Svg.lineSegment2d
                [ Attributes.stroke "black"
                , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth
                ]
        , segment
            |> Svg.lineSegment2d
                [ Attributes.stroke "white"
                , Attributes.strokeWidth <| String.fromFloat <| options.lineWidth - 2 * options.strokeWidth
                , Attributes.strokeLinecap "round"
                ]
        ]
      ]
        |> List.concat
    )
