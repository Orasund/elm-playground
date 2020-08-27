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
        [ Point2d.pixels 0 (zoom * (-(View.relative 15) / 2))
        , Point2d.pixels 0 (zoom * (View.relative 15 / 2))
        , Point2d.pixels (View.width / 2) (View.height / 2 + zoom * (View.relative 15 / 2))
        , Point2d.pixels (View.width / 2) (View.height / 2 + zoom * (-(View.relative 15) / 2))
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
            [ Attributes.fillOpacity <| "0.4"
            , Attributes.fill <| color
            ]


viewVBackground : { color : String, vMirror : Bool, hMirror : Bool } -> Svg msg
viewVBackground { color, vMirror, hMirror } =
    let
        zoom =
            1
    in
    Polygon2d.singleLoop
        [ Point2d.pixels (zoom * (-(View.relative 15) / 2)) 0
        , Point2d.pixels (zoom * (View.relative 15 / 2)) 0
        , Point2d.pixels (View.width / 2 + zoom * (View.relative 15 / 2)) (View.height / 2)
        , Point2d.pixels (View.width / 2 + zoom * (-(View.relative 15) / 2)) (View.height / 2)
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
            [ Attributes.fillOpacity <| "0.4"
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
            [ viewVBackground
                { color = colorName c1
                , vMirror = False
                , hMirror = False
                }
            , viewVBackground
                { color = colorName c2
                , vMirror = True
                , hMirror = True
                }
            , viewVBackground
                { color = colorName c1
                , vMirror = False
                , hMirror = True
                }
            , viewVBackground
                { color = colorName c2
                , vMirror = True
                , hMirror = False
                }
            ]
                ++ (Card.lines c1
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = False
                                    , hMirror = False
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c2
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = True
                                    , hMirror = True
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c1
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = False
                                    , hMirror = True
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c2
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = True
                                    , hMirror = False
                                    , color = color
                                    }
                            )
                   )
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
            [ viewHBackground
                { color = colorName c2
                , vMirror = False
                , hMirror = False
                }
            , viewHBackground
                { color = colorName c1
                , vMirror = True
                , hMirror = True
                }
            , viewHBackground
                { color = colorName c1
                , vMirror = False
                , hMirror = True
                }
            , viewHBackground
                { color = colorName c2
                , vMirror = True
                , hMirror = False
                }
            ]
                ++ (Card.lines c2
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewHorizontal
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = False
                                    , hMirror = False
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c1
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewHorizontal
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = True
                                    , hMirror = True
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c1
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewHorizontal
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = False
                                    , hMirror = True
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c2
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewHorizontal
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = True
                                    , hMirror = False
                                    , color = color
                                    }
                            )
                   )
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
            ]
                ++ (Card.lines c1
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = False
                                    , hMirror = False
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c2
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = False
                                    , hMirror = True
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c2
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = True
                                    , hMirror = False
                                    , color = color
                                    }
                            )
                   )
                ++ (Card.lines c1
                        |> List.map
                            (\{ offset, stroke, color } ->
                                viewVertical
                                    { offset = offset
                                    , stroke = stroke
                                    , vMirror = True
                                    , hMirror = True
                                    , color = color
                                    }
                            )
                   )
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
