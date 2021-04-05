module HermeticMind.View.BraidSigil exposing (drawPoints, init, initCircle, line, pointSize, points, strokeWidth, view)

import Angle
import Arc2d
import Circle2d
import Direction2d
import Geometry.Svg as Svg
import HermeticMind.Data.Alphabet as Alphabet exposing (TwentySix)
import HermeticMind.Data.Geometry as Geometry
import HermeticMind.Data.Turtle as Turtle exposing (Turtle)
import HermeticMind.View.BinarySigil as Sigil
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import Pixels
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index)
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


initCircle : { width : Float, height : Float, radius : Float } -> Index N -> List (Svg msg)
initCircle options startIndex =
    let
        p1 =
            points options |> StaticArray.get startIndex
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
        options =
            { width = width
            , height = height
            , radius = radius
            }

        p1 =
            points options |> StaticArray.get startIndex

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
                        Turtle.arcRightTo
                            { direction = Direction2d.positiveY
                            , radius = lineWidth / 2
                            }

                    else
                        Turtle.arcLeftTo
                            { direction = Direction2d.positiveY
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
                Turtle.arcRightTo

            else
                Turtle.arcLeftTo
    in
    if v1 == -1 then
        turtle
            |> rotate
                { direction = turtle.direction
                , radius =
                    turtle.position
                        |> Point2d.distanceFrom p1
                        |> Quantity.unwrap
                }

    else
        turtle
            |> rotate
                { direction = turtle.direction |> Direction2d.reverse
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
                            { direction = turtle.direction
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
                    Turtle.arcRightTo

                else
                    Turtle.arcLeftTo

            ( turtle, drawing ) =
                state.turtle
                    |> rotate
                        { direction = endDirection
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


view :
    { width : Float
    , height : Float
    , radius : Float
    , zoom : Float
    , asAlphabet : Char -> Index TwentySix
    , withCircle : Bool
    , debugMode : Bool
    , withRunes : Bool
    , withText : Bool
    , withBorder : Bool
    }
    -> String
    -> Html msg
view { width, height, radius, withText, asAlphabet, withCircle, debugMode, withBorder, zoom, withRunes } string =
    let
        options =
            { width = width
            , height = height
            , radius = radius
            }

        list =
            string
                |> String.toList
                |> List.map asAlphabet

        uniqueList =
            list |> List.uniqueBy Index.toInt

        border =
            { position = Point2d.unsafe { x = width / 2, y = lineWidth * 2 }
            , direction = Direction2d.positiveX
            , lineFun =
                \{ from, to } ->
                    let
                        segment =
                            LineSegment2d.from from to
                    in
                    [ segment
                        |> Svg.lineSegment2d
                            [ Attributes.stroke "black"
                            , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
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
                            , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                            ]
                    ]
            }
                |> Turtle.forwardBy (width / 2 - lineWidth * 2 + pointSize)
                |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.positiveY, radius = pointSize })
                |> Turtle.andThen (Turtle.forwardBy (height - lineWidth * 4 + pointSize))
                |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.negativeX, radius = pointSize })
                |> Turtle.andThen (Turtle.forwardBy (width - lineWidth * 4 + pointSize))
                |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.negativeY, radius = pointSize })
                |> Turtle.andThen (Turtle.forwardBy (height - lineWidth * 4 + pointSize))
                |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.positiveX, radius = pointSize })
                |> Turtle.andThen (Turtle.forwardBy (width / 2 - lineWidth * 2 + pointSize))
                |> Tuple.second
    in
    [ case ( list, uniqueList ) of
        ( head :: second :: thrid :: tail, _ :: distinctSecond :: distinctThird :: _ ) ->
            --nextIndex
            --::
            tail
                |> List.foldl
                    (\i2 ( state, out ) ->
                        let
                            ( newState, newOut ) =
                                line options
                                    state
                                    i2
                        in
                        ( newState
                        , newOut ++ out
                        )
                    )
                    (let
                        ( initState, startingDrawing ) =
                            init
                                { width = options.width
                                , height = options.height
                                , radius = options.radius
                                , startIndex = head
                                , nextIndex = second
                                , distinctSecond = distinctSecond
                                , distinctThird = distinctThird
                                }

                        ( newState, newOut ) =
                            line options initState thrid

                        --head
                     in
                     ( newState
                     , newOut
                        ++ (startingDrawing |> List.map (Svg.map never))
                     )
                    )
                |> (\( state, out ) ->
                        let
                            ( newState, newOut ) =
                                line options
                                    state
                                    head
                        in
                        ( newState
                        , newOut ++ out
                        )
                   )
                |> (\( state, out ) ->
                        (if state.startIndex == head then
                            line options
                                { state
                                    | visited = state.visited |> StaticArray.set head -1
                                }
                                head
                                |> Tuple.second

                         else
                            line options
                                { state
                                    | visited = state.visited |> StaticArray.set head -1
                                }
                                head
                                |> Tuple.second
                        )
                            ++ out
                            ++ initCircle options head
                   )
                |> (if withCircle then
                        List.append
                            (Circle2d.atPoint (Point2d.pixels (width / 2) (height / 2))
                                (Pixels.pixels <| radius)
                                |> Svg.circle2d
                                    [ Attributes.fill <| "none"
                                    , Attributes.stroke <| "black"
                                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                                    ]
                                |> List.singleton
                            )

                    else
                        identity
                   )

        _ ->
            []
    ]
        |> List.concat
        |> List.append
            (if debugMode then
                drawPoints options

             else
                []
            )
        |> List.append
            (if withRunes then
                Index.range n
                    |> List.map
                        (\r ->
                            let
                                symbolLength =
                                    4
                            in
                            { value = Index.toInt r
                            , size = symbolLength
                            , color = "black"
                            , radius = 1
                            , strokeWidth = 1 / 8
                            , point =
                                Point2d.pixels (width / 2) (height / 2)
                                    |> Point2d.translateBy (Vector2d.pixels (radius * 1.25) 0)
                                    |> Point2d.rotateAround (Point2d.pixels (width / 2) (height / 2))
                                        (Angle.radians <| (2 * pi / toFloat (Length.toInt n)) * (0.5 + toFloat (Index.toInt r)))
                            }
                                |> Sigil.view
                        )
                    |> List.concat

             else
                []
            )
        |> List.append
            (if withText then
                [ Svg.text_
                    [ Attributes.fontFamily "Dancing Script, serif"
                    , width / 2 |> String.fromFloat |> Attributes.x
                    , height - lineWidth * 8 |> String.fromFloat |> Attributes.y
                    , Attributes.textAnchor "middle"
                    , Attributes.alignmentBaseline "central"
                    ]
                    [ Svg.text string ]
                ]

             else
                []
            )
        |> List.append
            (if withBorder then
                border

             else
                []
            )
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| zoom * width) ++ "px"
            , Attributes.height <| (String.fromFloat <| zoom * height) ++ "px"
            , Attributes.version <| "1.1"
            , Attributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat width
                    ++ " "
                    ++ String.fromFloat height
            ]
