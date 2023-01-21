module Chapter.IdealPoints exposing (..)

import ElmBook.Chapter exposing (Chapter)
import Html exposing (Html)
import Hyperbolic exposing (IdealPoint, Line)
import View.Canvas


triangleWithPoints : IdealPoint -> IdealPoint -> IdealPoint -> List Line
triangleWithPoints p1 p2 p3 =
    [ Hyperbolic.lineFromIdealPoints p1 p2
    , Hyperbolic.lineFromIdealPoints p2 p3
    , Hyperbolic.lineFromIdealPoints p3 p1
    ]


triangle : Html msg
triangle =
    triangleWithPoints (Hyperbolic.pointAtInfinity 0)
        (Hyperbolic.pointAtInfinity (2 * pi / 3))
        (Hyperbolic.pointAtInfinity (4 * pi / 3))
        |> List.concatMap (Hyperbolic.pointsAlongLine 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


triangleWithPerpendicularLine : Html msg
triangleWithPerpendicularLine =
    triangleWithPoints (Hyperbolic.pointAtInfinity 0)
        (Hyperbolic.pointAtInfinity (2 * pi / 3))
        (Hyperbolic.pointAtInfinity (4 * pi / 3))
        |> List.concatMap
            (\(( p0, p1 ) as line) ->
                line
                    |> Hyperbolic.perpendicularLineThrough
                        Hyperbolic.origin
                    |> Maybe.andThen
                        (\perpenticular ->
                            Hyperbolic.intersectLines line perpenticular
                                |> Maybe.map (Hyperbolic.nearestIdealPointOf perpenticular)
                                |> Maybe.map
                                    (\p2 ->
                                        [ perpenticular
                                        , ( p0, p2 )
                                        , ( p2, p1 )
                                        ]
                                    )
                        )
                    |> Maybe.withDefault []
                    |> (::) line
            )
        |> List.concatMap (Hyperbolic.pointsAlongLine 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


recursiveTriangle : Html msg
recursiveTriangle =
    let
        newLines ( p1, p2 ) =
            ( p1, p2 )
                |> Hyperbolic.perpendicularLineThrough
                    Hyperbolic.origin
                |> Maybe.map
                    (\line ->
                        p1
                            |> Hyperbolic.unsafeFromIdealPoint
                            |> Hyperbolic.nearestIdealPointOf line
                            |> (\p3 ->
                                    [ ( p1, p3 ), ( p3, p2 ) ]
                               )
                    )
                |> Maybe.withDefault []

        iterate ( input, output ) =
            ( input |> List.concatMap newLines
            , input ++ output
            )
    in
    List.range 0 4
        |> List.foldl (\_ -> iterate)
            ( triangleWithPoints (Hyperbolic.pointAtInfinity 0)
                (Hyperbolic.pointAtInfinity (2 * pi / 3))
                (Hyperbolic.pointAtInfinity (4 * pi / 3))
            , []
            )
        |> (\( input, output ) -> input ++ output)
        |> List.concatMap (Hyperbolic.pointsAlongLine 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


chapter : Chapter state
chapter =
    ElmBook.Chapter.chapter "Working with ideal points"
        |> ElmBook.Chapter.withComponentList
            [ ( "Triangle into infinity", triangle )
            , ( "Infinite triangles", recursiveTriangle )
            , ( "Perpendicular lines through each line of the triangle", triangleWithPerpendicularLine )
            ]
        |> ElmBook.Chapter.render
            """
Let's try to draw some images with the knowledge we learned so far.

For now we will only use ideal points (points at infinity) to construct lines.

We start by drawing a triangle with ideal points as vertices.

```
triangleWithPoints : IdealPoint -> IdealPoint -> IdealPoint -> List Line
triangleWithPoints p1 p2 p3 =
    [ Hyperbolic.lineFromIdealPoints p1 p2
    , Hyperbolic.lineFromIdealPoints p2 p3
    , Hyperbolic.lineFromIdealPoints p3 p1
    ]


triangleWithPoints (Hyperbolic.pointAtInfinity 0)
    (Hyperbolic.pointAtInfinity (2 * pi / 3))
    (Hyperbolic.pointAtInfinity (4 * pi / 3))
    |> List.concatMap (Hyperbolic.pointsAlongLine 100)
    |> List.map Hyperbolic.projectOntoPoincareDisc
```

<component with-label="Triangle into infinity"/>

We now want construct new triangles from the lines of the old triangle.

```
triangleWithPerpendicularLine : Html msg
triangleWithPerpendicularLine =
    triangleWithPoints (Hyperbolic.pointAtInfinity 0)
        (Hyperbolic.pointAtInfinity (2 * pi / 3))
        (Hyperbolic.pointAtInfinity (4 * pi / 3))
        |> List.concatMap
            (\\(( p0, p1 ) as line) ->
                line
                    |> Hyperbolic.perpendicularLineThrough
                        Hyperbolic.origin
                    |> Maybe.andThen
                        (\\perpenticular ->
                            Hyperbolic.intersectLines line perpenticular
                                |> Maybe.map (Hyperbolic.nearestIdealPointOf perpenticular)
                                |> Maybe.map
                                    (\\p2 ->
                                        [ perpenticular
                                        , ( p0, p2 )
                                        , ( p2, p1 )
                                        ]
                                    )
                        )
                    |> Maybe.withDefault []
                    |> (::) line
            )
        |> List.concatMap (Hyperbolic.pointsAlongLine 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic
```

<component with-label="Perpendicular lines through each line of the triangle"/>

To do so, we will draw the perpendicular line through the origin (the center) and intersect it with our line.
Given the intersection point, we can now find the nearer ideal point on the perpendicular line and construct two new lines from it.

Notice, how we do not use any measurement of degrees. This is because working with angles in the hyperbolic disc can be tricky. We therefore avoid them it for now.

If we repeat the process we end up with a rather pleasing image.

<component with-label="Infinite triangles"/>
"""
