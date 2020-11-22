module Sprawlopolis.Box exposing (Box, BoxContent(..), draw)

import Circle2d
import Geometry.Svg as Svg
import Pixels
import Point2d
import Polygon2d
import Polyline2d exposing (Polyline2d)
import Sprawlopolis.Color exposing (Color(..))
import Sprawlopolis.Rule as Rule
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
    | Rule Int


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

                            Rule int ->
                                let
                                    circleSize =
                                        View.width / 4 - View.relative 1
                                in
                                Rule.view int
                                    |> (\( rule1, rule2 ) ->
                                            if rule2 == [] then
                                                [ [ Circle2d.atPoint
                                                        (Point2d.pixels (xOffset + View.width / 4) (yOffset + (View.height / 4)))
                                                        (Pixels.pixels <| circleSize)
                                                        |> Svg.circle2d
                                                            [ Attributes.fill <|
                                                                "white"
                                                            , Attributes.fillOpacity <| "0.8"
                                                            ]
                                                  ]
                                                , rule1
                                                    |> List.indexedMap
                                                        (\i string ->
                                                            string
                                                                |> Svg.text
                                                                |> List.singleton
                                                                |> Svg.text_
                                                                    [ Attributes.x <| String.fromFloat <| xOffset + View.width / 4
                                                                    , Attributes.y <| String.fromFloat <| yOffset + View.height / 4 - View.relative 5 + View.relative 2 * toFloat i
                                                                    , Attributes.textAnchor <| "middle"
                                                                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative 2) ++ "px sans-serif"
                                                                    , Attributes.fill <| "black"
                                                                    ]
                                                        )
                                                ]
                                                    |> List.concat

                                            else
                                                [ [ Circle2d.atPoint
                                                        (Point2d.pixels (xOffset + View.width / 4) (yOffset + (View.height / 4) + circleSize))
                                                        (Pixels.pixels <| circleSize)
                                                        |> Svg.circle2d
                                                            [ Attributes.fill <|
                                                                "white"
                                                            , Attributes.fillOpacity <| "0.8"
                                                            ]
                                                  , Circle2d.atPoint
                                                        (Point2d.pixels (xOffset + View.width / 4) (yOffset + (View.height / 4) - circleSize))
                                                        (Pixels.pixels <| circleSize)
                                                        |> Svg.circle2d
                                                            [ Attributes.fill <|
                                                                "white"
                                                            , Attributes.fillOpacity <| "0.8"
                                                            ]
                                                  ]
                                                , rule1
                                                    |> List.indexedMap
                                                        (\i string ->
                                                            string
                                                                |> Svg.text
                                                                |> List.singleton
                                                                |> Svg.text_
                                                                    [ Attributes.x <| String.fromFloat <| xOffset + View.width / 4
                                                                    , Attributes.y <| String.fromFloat <| yOffset + View.relative 2 * toFloat (i + 2)
                                                                    , Attributes.textAnchor <| "middle"
                                                                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative 2) ++ "px sans-serif"
                                                                    , Attributes.fill <| "black"
                                                                    ]
                                                        )
                                                , rule2
                                                    |> List.indexedMap
                                                        (\i string ->
                                                            string
                                                                |> Svg.text
                                                                |> List.singleton
                                                                |> Svg.text_
                                                                    [ Attributes.x <| String.fromFloat <| xOffset + View.width / 4
                                                                    , Attributes.y <| String.fromFloat <| yOffset + View.height / 2 - View.relative 14 + View.relative 2 * toFloat (i + 2)
                                                                    , Attributes.textAnchor <| "middle"
                                                                    , Attributes.style <| "font: " ++ (String.fromFloat <| View.relative 2) ++ "px sans-serif"
                                                                    , Attributes.fill <| "black"
                                                                    ]
                                                        )
                                                ]
                                                    |> List.concat
                                       )
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
