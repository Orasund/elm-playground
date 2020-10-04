module Sprawlopolis.Box exposing (Box, BoxContent(..), draw)

import Geometry.Svg as Svg
import LineSegment2d
import Point2d
import Polygon2d
import Polyline2d exposing (Polyline2d)
import Sprawlopolis.Color as Color exposing (Color(..))
import Sprawlopolis.View as View
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type BoxContent
    = HStreet
    | VStreet
    | NECurve
    | SECurve
    | SWCurve
    | NWCurve


type alias Box =
    { color : Color
    , withXOffset : Bool
    , withYOffset : Bool
    , content : Maybe BoxContent
    }


draw : Box -> List (Svg msg)
draw { color, withXOffset, withYOffset, content } =
    let
        xOffset =
            if withXOffset then
                View.width / 2

            else
                0

        yOffset =
            if withYOffset then
                View.height / 2

            else
                0

        streetWidth =
            View.relative 4
    in
    (Polygon2d.singleLoop
        [ Point2d.pixels xOffset yOffset
        , Point2d.pixels (xOffset + View.width / 2) yOffset
        , Point2d.pixels (xOffset + View.width / 2) (yOffset + View.height / 2)
        , Point2d.pixels xOffset (yOffset + View.height / 2)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <|
                case color of
                    G ->
                        "url(#park)"

                    R ->
                        "url(#residential)"

                    Y ->
                        "url(#industrial)"

                    B ->
                        "url(#commercial)"
            ]
    )
        :: (content
                |> Maybe.map
                    (\c ->
                        case c of
                            HStreet ->
                                Polyline2d.fromVertices
                                    [ Point2d.pixels xOffset (yOffset + View.height / 4)
                                    , Point2d.pixels (xOffset + View.width / 2) (yOffset + View.height / 4)
                                    ]
                                    |> street

                            VStreet ->
                                Polyline2d.fromVertices
                                    [ Point2d.pixels (xOffset + View.width / 4) yOffset
                                    , Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 2)
                                    ]
                                    |> street

                            NECurve ->
                                Polyline2d.fromVertices
                                    [ Point2d.pixels (xOffset + View.width / 4) yOffset
                                    , Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 4)
                                    , Point2d.pixels (xOffset + View.width / 2) (yOffset + View.height / 4)
                                    ]
                                    |> street

                            SECurve ->
                                Polyline2d.fromVertices
                                    [ Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 2)
                                    , Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 4)
                                    , Point2d.pixels (xOffset + View.width / 2) (yOffset + View.height / 4)
                                    ]
                                    |> street

                            SWCurve ->
                                Polyline2d.fromVertices
                                    [ Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 2)
                                    , Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 4)
                                    , Point2d.pixels xOffset (yOffset + View.height / 4)
                                    ]
                                    |> street

                            NWCurve ->
                                Polyline2d.fromVertices
                                    [ Point2d.pixels (xOffset + View.width / 4) yOffset
                                    , Point2d.pixels (xOffset + View.width / 4) (yOffset + View.height / 4)
                                    , Point2d.pixels xOffset (yOffset + View.height / 4)
                                    ]
                                    |> street
                    )
                |> Maybe.withDefault []
           )


street : Polyline2d units coordinates -> List (Svg msg)
street line =
    let
        streetWidth =
            View.relative 4
    in
    [ line
        |> Svg.polyline2d
            [ Attributes.strokeWidth <| String.fromFloat <| streetWidth + (View.relative 1 / 2)
            , Attributes.stroke <| "black"
            , Attributes.strokeLinecap <| "butt"
            , Attributes.fill <| "none"
            ]
    , line
        |> Svg.polyline2d
            [ Attributes.strokeWidth <| String.fromFloat <| streetWidth
            , Attributes.stroke <| "white"
            , Attributes.strokeLinecap <| "butt"
            , Attributes.fill <| "none"
            ]
    , line
        |> Svg.polyline2d
            [ Attributes.strokeDasharray <|
                (String.fromFloat <| View.relative <| 1 / 2)
                    ++ " "
                    ++ (String.fromFloat <| View.relative <| 2)
            , Attributes.strokeWidth <| String.fromFloat <| View.relative 1 / 2
            , Attributes.stroke <| "black"
            , Attributes.strokeLinecap <| "square"
            , Attributes.fill <| "none"
            ]
    ]
