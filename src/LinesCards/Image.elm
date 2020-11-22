module LinesCards.Image exposing (view)

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d
import Circle2d exposing (Circle2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import LinesCards.Card as Card exposing (Card(..), Color(..))
import LinesCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


bigRadius : Float
bigRadius =
    --View.radius * 5 / 4
    2 * View.radius / sqrt 2


viewVertical : { color : String, vMirror : Bool, hMirror : Bool, offset : Float, stroke : Float } -> Svg msg
viewVertical { color, vMirror, hMirror, offset, stroke } =
    Polygon2d.singleLoop
        [ Point2d.pixels (offset - stroke / 2) 0
        , Point2d.pixels (offset + stroke / 2) 0
        , Point2d.pixels (View.width / 2 + offset + stroke / 2) (View.height / 2)
        , Point2d.pixels (View.width / 2 + offset - stroke / 2) (View.height / 2)
        ]
        |> (if hMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.x
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> (if vMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.y
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.polygon2d
            [ Attributes.strokeLinecap <| "round"
            , Attributes.fillOpacity <| "0.8"
            , Attributes.fill <| color
            ]


viewHorizontal : { color : String, vMirror : Bool, hMirror : Bool, offset : Float, stroke : Float } -> Svg msg
viewHorizontal { color, vMirror, hMirror, offset, stroke } =
    let
        zoom =
            View.relative 1
    in
    Polygon2d.singleLoop
        [ Point2d.pixels 0 (zoom * (offset - stroke / 2))
        , Point2d.pixels 0 (zoom * (offset + stroke / 2))
        , Point2d.pixels (View.width / 2) (View.height / 2 + zoom * (offset + stroke / 2))
        , Point2d.pixels (View.width / 2) (View.height / 2 + zoom * (offset - stroke / 2))
        ]
        |> (if hMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.x
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> (if vMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.y
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.polygon2d
            [ Attributes.strokeLinecap <| "round"
            , Attributes.fillOpacity <| "0.8"
            , Attributes.fill <| color
            ]


viewHBackground : { color : String, vMirror : Bool, hMirror : Bool } -> Svg msg
viewHBackground { color, vMirror, hMirror } =
    let
        zoom =
            View.relative 1
    in
    Polygon2d.singleLoop
        [ Point2d.pixels 0 (zoom * (-(View.relative 7) / 2))
        , Point2d.pixels 0 (zoom * (View.relative 7 / 2))
        , Point2d.pixels (View.width / 2) (View.height / 2 + zoom * (View.relative 7 / 2))
        , Point2d.pixels (View.width / 2) (View.height / 2 + zoom * (-(View.relative 7) / 2))
        ]
        |> (if hMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.x
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> (if vMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.y
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.polygon2d
            [ Attributes.fillOpacity <| "0.6"
            , Attributes.fill <| color
            ]


viewHCircle : { dashes : String, color : String, mirror : Bool, size : Float } -> Svg msg
viewHCircle { dashes, color, mirror, size } =
    let
        yOffset =
            View.width / 2 + View.relative 5
    in
    Arc2d.with
        { centerPoint = Point2d.pixels (View.width / 2) (((View.height / 2) ^ 2 + yOffset ^ 2) / (2 * yOffset) - yOffset)
        , radius = Pixels.pixels <| ((View.width / 2) ^ 2 + yOffset ^ 2) / (2 * yOffset) - View.relative 1.5
        , startAngle = Angle.radians <| 0
        , sweptAngle = Angle.radians <| 2 * pi
        }
        |> (if mirror then
                Arc2d.mirrorAcross
                    (Axis2d.x
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.arc2d
            [ Attributes.strokeDasharray <| dashes
            , Attributes.stroke <| color
            , Attributes.strokeWidth <| String.fromFloat <| size
            , Attributes.fill "transparent"
            , Attributes.strokeLinecap <| "round"
            ]


viewVCircle : { dashes : String, color : String, mirror : Bool, size : Float } -> Svg msg
viewVCircle { dashes, color, mirror, size } =
    let
        xOffset =
            View.width / 2 - View.relative 5 - View.relative 1.5
    in
    Arc2d.with
        { centerPoint = Point2d.pixels -(((View.height / 2) ^ 2 + xOffset ^ 2) / (2 * xOffset) - xOffset) (View.height / 2)
        , radius = Pixels.pixels <| ((View.height / 2) ^ 2 + xOffset ^ 2) / (2 * xOffset)
        , startAngle = Angle.radians <| -pi / 2
        , sweptAngle = Angle.radians <| 2 * pi
        }
        |> (if mirror then
                Arc2d.mirrorAcross
                    (Axis2d.y
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.arc2d
            [ Attributes.strokeDasharray <| dashes
            , Attributes.stroke <| color
            , Attributes.strokeWidth <| String.fromFloat <| size
            , Attributes.fill "transparent"
            , Attributes.strokeLinecap <| "round"
            ]


viewLine : { color : Color, vMirror : Bool, hMirror : Bool } -> Svg msg
viewLine { color, vMirror, hMirror } =
    LineSegment2d.from (Point2d.pixels 0 0) (Point2d.pixels (View.width / 2) (View.height / 2))
        |> (if hMirror then
                LineSegment2d.mirrorAcross
                    (Axis2d.x
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> (if vMirror then
                LineSegment2d.mirrorAcross
                    (Axis2d.y
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.lineSegment2d
            [ Attributes.strokeDasharray <| Card.dashes color
            , Attributes.strokeWidth <| String.fromFloat <| View.relative 1
            , Attributes.stroke <| "black"
            , Attributes.strokeLinecap <| "round"
            , Attributes.fill <| "none"
            ]


viewVBackground : { color : String, vMirror : Bool, hMirror : Bool } -> Svg msg
viewVBackground { color, vMirror, hMirror } =
    let
        zoom =
            1
    in
    Polygon2d.singleLoop
        [ Point2d.pixels (zoom * (-(View.relative 7) / 2)) 0
        , Point2d.pixels (zoom * (View.relative 7 / 2)) 0
        , Point2d.pixels (View.width / 2 + zoom * (View.relative 7 / 2)) (View.height / 2)
        , Point2d.pixels (View.width / 2 + zoom * (-(View.relative 7) / 2)) (View.height / 2)
        ]
        |> (if hMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.x
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> (if vMirror then
                Polygon2d.mirrorAcross
                    (Axis2d.y
                        |> Axis2d.moveTo (Point2d.pixels (View.width / 2) (View.height / 2))
                    )

            else
                identity
           )
        |> Svg.polygon2d
            [ Attributes.fillOpacity <| "0.6"
            , Attributes.fill <| color
            ]


viewVEdges : Color -> Color -> List (Svg msg)
viewVEdges c1 c2 =
    let
        colorName : Color -> String
        colorName c =
            case c of
                Y ->
                    View.yellow

                G ->
                    View.green

                B ->
                    View.blue

                R ->
                    View.red

        form =
            [ viewVCircle
                { dashes = ""
                , color = colorName c1
                , mirror = False
                , size = View.relative 6
                }
            , viewVCircle
                { dashes = Card.dashes c1
                , color = "black"
                , mirror = False
                , size = View.relative 1
                }
            , viewVCircle
                { dashes = ""
                , color = colorName c2
                , mirror = True
                , size = View.relative 6
                }
            , viewVCircle
                { dashes = Card.dashes c2
                , color = "black"
                , mirror = True
                , size = View.relative 1
                }
            ]
    in
    form ++ form


viewHEdges : Color -> Color -> List (Svg msg)
viewHEdges c1 c2 =
    let
        colorName : Color -> String
        colorName c =
            case c of
                Y ->
                    View.yellow

                G ->
                    View.green

                B ->
                    View.blue

                R ->
                    View.red

        form =
            [ viewHCircle
                { dashes = ""
                , color = colorName c1
                , mirror = True
                , size = View.relative 6
                }
            , viewHCircle
                { dashes = Card.dashes c1
                , color = "black"
                , mirror = True
                , size = View.relative 1
                }
            , viewHCircle
                { dashes = ""
                , color = colorName c2
                , mirror = False
                , size = View.relative 6
                }
            , viewHCircle
                { dashes = Card.dashes c2
                , color = "black"
                , mirror = False
                , size = View.relative 1
                }
            ]
    in
    form ++ form


viewCross : Color -> Color -> List (Svg msg)
viewCross c1 c2 =
    let
        colorName : Color -> String
        colorName c =
            case c of
                Y ->
                    View.yellow

                G ->
                    View.green

                B ->
                    View.blue

                R ->
                    View.red

        form =
            [ viewHBackground
                { color = colorName c1
                , vMirror = False
                , hMirror = False
                }
            , viewHBackground
                { color = colorName c2
                , vMirror = False
                , hMirror = True
                }
            , viewHBackground
                { color = colorName c2
                , vMirror = True
                , hMirror = False
                }
            , viewHBackground
                { color = colorName c1
                , vMirror = True
                , hMirror = True
                }
            , viewLine
                { color = c1
                , vMirror = False
                , hMirror = False
                }
            , viewLine
                { color = c2
                , vMirror = False
                , hMirror = True
                }
            , viewLine
                { color = c2
                , vMirror = True
                , hMirror = False
                }
            , viewLine
                { color = c1
                , vMirror = True
                , hMirror = True
                }
            ]
    in
    form ++ form


view : Card -> List (Svg msg)
view card =
    case card of
        Cross c1 c2 ->
            viewCross c1 c2

        VEdges c1 c2 ->
            viewVEdges c1 c2

        HEdges c1 c2 ->
            viewHEdges c1 c2
