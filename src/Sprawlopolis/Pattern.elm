module Sprawlopolis.Pattern exposing (patterns)

import Geometry.Svg as Svg
import Point2d
import Polygon2d
import Sprawlopolis.Color as Color exposing (Color(..))
import Sprawlopolis.View as View
import Svg exposing (Svg)
import Svg.Attributes as Attributes


patterns : List (Svg msg)
patterns =
    [ park
        |> asPattern
        |> Svg.pattern
            [ Attributes.id "park"
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.width <| String.fromFloat <| View.relative 5
            , Attributes.height <| String.fromFloat <| View.relative 5
            , Attributes.patternUnits "userSpaceOnUse"
            ]
    , residential
        |> asPattern
        |> Svg.pattern
            [ Attributes.id "residential"
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.width <| String.fromFloat <| View.relative 5
            , Attributes.height <| String.fromFloat <| View.relative 5
            , Attributes.patternUnits "userSpaceOnUse"
            ]
    , industrial
        |> asPattern
        |> Svg.pattern
            [ Attributes.id "industrial"
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.width <| String.fromFloat <| View.relative 5
            , Attributes.height <| String.fromFloat <| View.relative 5
            , Attributes.patternUnits "userSpaceOnUse"
            ]
    , commercial
        |> asPattern
        |> Svg.pattern
            [ Attributes.id "commercial"
            , Attributes.x "0"
            , Attributes.y "0"
            , Attributes.width <| String.fromFloat <| View.relative 5
            , Attributes.height <| String.fromFloat <| View.relative 5
            , Attributes.patternUnits "userSpaceOnUse"
            ]
    ]


asPattern : ({ x : Float, y : Float, size : Float } -> List (Svg msg)) -> List (Svg msg)
asPattern fun =
    List.concat
        [ fun { x = 2.5, y = 2.5, size = 3 }
        , fun { x = 0, y = 0, size = 3 }
        , fun { x = 0, y = 5, size = 3 }
        , fun { x = 5, y = 0, size = 3 }
        , fun { x = 5, y = 5, size = 3 }
        ]


industrial : { x : Float, y : Float, size : Float } -> List (Svg msg)
industrial { x, y, size } =
    [ Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y - (size / 5) * 1)
        , Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y + (size / 5) * 1)
        , Point2d.pixels (View.relative <| x) (View.relative <| y + (size / 5) * 1)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| Y
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x - (size / 5) * 1) (View.relative <| y - (size / 5) * 1)
        , Point2d.pixels (View.relative <| x - (size / 5) * 1) (View.relative <| y + (size / 5) * 1)
        , Point2d.pixels (View.relative <| x + (size / 5) * 1) (View.relative <| y + (size / 5) * 1)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| Y
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x) (View.relative <| y - (size / 5) * 1)
        , Point2d.pixels (View.relative <| x) (View.relative <| y + (size / 5) * 1)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y + (size / 5) * 1)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| Y
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y)
        , Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| Y
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x + (size / 5) * 1) (View.relative <| y - (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y - (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 1) (View.relative <| y + (size / 5) * 2)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| Y
            ]
    ]


residential : { x : Float, y : Float, size : Float } -> List (Svg msg)
residential { x, y, size } =
    [ Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y - (size / 5) * 3 / 2)
        , Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y - (size / 5) * 3 / 2)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| R
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x) (View.relative <| y - (size / 5) * 3)
        , Point2d.pixels (View.relative <| x - (size / 5) * 3) (View.relative <| y - (size / 5) * 1)
        , Point2d.pixels (View.relative <| x + (size / 5) * 3) (View.relative <| y - (size / 5) * 1)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| R
            ]
    ]


park : { x : Float, y : Float, size : Float } -> List (Svg msg)
park { x, y, size } =
    [ Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x) (View.relative <| y - (size / 5) * 3)
        , Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| G
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x) (View.relative <| y - (size / 5) * 2)
        , Point2d.pixels (View.relative <| x - (size / 5) * 2) (View.relative <| y + (size / 5) * 1)
        , Point2d.pixels (View.relative <| x + (size / 5) * 2) (View.relative <| y + (size / 5) * 1)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| G
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x - (size / 5) * 1 / 2) (View.relative <| y)
        , Point2d.pixels (View.relative <| x - (size / 5) * 1 / 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 1 / 2) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 1 / 2) (View.relative <| y)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| G
            ]
    ]


commercial : { x : Float, y : Float, size : Float } -> List (Svg msg)
commercial { x, y, size } =
    [ Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x - (size / 5) * 1 / 2) (View.relative <| y - (size / 5) * 3)
        , Point2d.pixels (View.relative <| x) (View.relative <| y - (size / 5) * 1)
        , Point2d.pixels (View.relative <| x + (size / 5) * 1 / 2) (View.relative <| y - (size / 5) * 3)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| B
            ]
    , Polygon2d.singleLoop
        [ Point2d.pixels (View.relative <| x) (View.relative <| y - (size / 5) * 3)
        , Point2d.pixels (View.relative <| x - (size / 5) * 1) (View.relative <| y + (size / 5) * 1)
        , Point2d.pixels (View.relative <| x) (View.relative <| y + (size / 5) * 2)
        , Point2d.pixels (View.relative <| x + (size / 5) * 1) (View.relative <| y + (size / 5) * 1)
        ]
        |> Svg.polygon2d
            [ Attributes.fill <| Color.toString <| B
            ]
    ]
