module Chapter.LineSegments exposing (chapter)

import ElmBook.Chapter exposing (Chapter)
import Html exposing (Html)
import Hyperbolic
import View.Canvas


asPolygon list =
    case list of
        head :: tail ->
            tail
                |> List.foldl
                    (\a ( b, output ) ->
                        ( a, ( a, b ) :: output )
                    )
                    ( head, [] )
                |> (\( a, l ) -> ( a, head ) :: l)

        [] ->
            []


polygon : Html msg
polygon =
    let
        n =
            3
    in
    List.range 0 (n - 1)
        |> List.map (\i -> toFloat i / toFloat n)
        |> List.map
            (\range ->
                Hyperbolic.fromPolarCoords
                    { radius = 1
                    , angle = range * 2 * pi
                    }
            )
        |> asPolygon
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


polygon2 : Html msg
polygon2 =
    let
        a1 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 0
                }

        a2 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 2 * pi / 3
                }

        a3 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 4 * pi / 3
                }

        init =
            [ ( ( a1, a2 ), a3 )
            , ( ( a2, a3 ), a1 )
            , ( ( a3, a1 ), a2 )
            ]
    in
    init
        |> List.concatMap
            (\( ( p1, p2 ), p3 ) ->
                Hyperbolic.lineFromPoints p1 p2
                    |> Maybe.map
                        (\line ->
                            p3
                                |> Hyperbolic.reflectBy line
                        )
                    |> Maybe.map
                        (\p4 ->
                            [ ( ( p1, p4 ), p2 )
                            , ( ( p4, p2 ), p1 )
                            ]
                        )
                    |> Maybe.withDefault []
            )
        |> (++) init
        |> List.map Tuple.first
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


polygon3 : Html msg
polygon3 =
    let
        radius =
            1

        a1 =
            Hyperbolic.fromPolarCoords
                { radius = radius
                , angle = 0
                }

        a2 =
            Hyperbolic.fromPolarCoords
                { radius = radius
                , angle = 2 * pi / 3
                }

        a3 =
            Hyperbolic.fromPolarCoords
                { radius = radius
                , angle = 4 * pi / 3
                }

        init =
            [ ( ( a1, a2 ), a3 )
            , ( ( a2, a3 ), a1 )
            , ( ( a3, a1 ), a2 )
            ]

        iterate =
            List.concatMap
                (\( ( p1, p2 ), p3 ) ->
                    Hyperbolic.lineFromPoints p1 p2
                        |> Maybe.map
                            (\line ->
                                p3
                                    |> Hyperbolic.reflectBy line
                            )
                        |> Maybe.map
                            (\p4 ->
                                [ ( ( p1, p4 ), p2 )
                                , ( ( p4, p2 ), p1 )
                                ]
                            )
                        |> Maybe.withDefault []
                )
    in
    List.range 0 2
        |> List.foldl
            (\_ ( list, output ) ->
                ( iterate list, list ++ output )
            )
            ( init, [] )
        |> (\( a, b ) -> a ++ b)
        |> List.map Tuple.first
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


chapter : Chapter state
chapter =
    ElmBook.Chapter.chapter "Line Segments"
        |> ElmBook.Chapter.withComponentList
            [ ( "polygon", polygon )
            , ( "polygon2", polygon2 )
            , ( "polygon3", polygon3 )
            ]
        |> ElmBook.Chapter.render """
Sofar we used ideal points to construct lines. Now we want to investigate how we can construct proper points and line segments between two proper points.

We have already seen that we can construct points by projecting them from the euclidean plane onto the hyperbolic disc.
Now we want to construct points directly in the hyperbolic plane. To do so, we can use polar coordinates

```
asPolygon list =
    case list of
        head :: tail ->
            tail
                |> List.foldl
                    (\\a ( b, output ) ->
                        ( a, ( a, b ) :: output )
                    )
                    ( head, [] )
                |> (\\( a, l ) -> ( a, head ) :: l)

        [] ->
            []


polygon : Html msg
polygon =
    let
        n =
            3
    in
    List.range 0 (n - 1)
        |> List.map (\\i -> toFloat i / toFloat n)
        |> List.map
            (\\range ->
                Hyperbolic.fromPolarCoords
                    { radius = 1
                    , angle = range * 2 * pi
                    }
            )
        |> asPolygon
        |> List.concatMap 
            (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
```

<component with-label="polygon"/>

We can now go ahead and mirror each point with the opposite line to get a new point.

```
polygon2 : Html msg
polygon2 =
    let
        a1 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 0
                }

        a2 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 2 * pi / 3
                }

        a3 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 4 * pi / 3
                }

        init =
            [ ( ( a1, a2 ), a3 )
            , ( ( a2, a3 ), a1 )
            , ( ( a3, a1 ), a2 )
            ]
    in
    init
        |> List.concatMap
            (\\( ( p1, p2 ), p3 ) ->
                Hyperbolic.lineFromPoints p1 p2
                    |> Maybe.map
                        (\\line ->
                            p3
                                |> Hyperbolic.reflectBy line
                        )
                    |> Maybe.map
                        (\\p4 ->
                            [ ( ( p1, p4 ), p2 )
                            , ( ( p4, p2 ), p1 )
                            ]
                        )
                    |> Maybe.withDefault []
            )
        |> (++) init
        |> List.map Tuple.first
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
```

<component with-label="polygon2"/>

We can go ahead an repeat that process a few times, and then the result looks something like

<component with-label="polygon3"/>

Isn't that odd, that the triangle seem to overlap. What did we do wrong?
Well the problem is, that we expected to see a tiling (=a structure that fills the entire disc) similar to the image the drew using the ideal points.

But the hyperbolic plane isn't as it seems. Mirroring a point does not return a new triangle with same size.
"""
