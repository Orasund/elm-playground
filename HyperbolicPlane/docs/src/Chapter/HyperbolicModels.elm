module Chapter.HyperbolicModels exposing (..)

import Canvas exposing (Renderable)
import Canvas.Settings
import Color
import ElmBook.Chapter exposing (Chapter)
import Html exposing (Html)
import Hyperbolic
import Layout
import View.Canvas


grid : List ( ( Float, Float ), ( Float, Float ) )
grid =
    List.range 0 20
        |> List.map (\i -> toFloat i / 20)
        |> List.concatMap
            (\ratio ->
                [ ( ( -1, ratio * 2 - 1 )
                  , ( 1, ratio * 2 - 1 )
                  )
                , ( ( ratio * 2 - 1, -1 )
                  , ( ratio * 2 - 1, 1 )
                  )
                ]
            )


euclideanCoords : Html msg
euclideanCoords =
    let
        pointsPerLine =
            100
    in
    grid
        |> List.concatMap
            (\( ( x1, y1 ), ( x2, y2 ) ) ->
                List.range 0 (pointsPerLine - 1)
                    |> List.map (\i -> toFloat i / toFloat (pointsPerLine - 1))
                    |> List.map
                        (\amount ->
                            ( x1 + amount * (x2 - x1)
                            , y1 + amount * (y2 - y1)
                            )
                        )
            )
        |> View.Canvas.euclidean


euclideanProjection : Html msg
euclideanProjection =
    grid
        |> List.map
            (Tuple.mapBoth Hyperbolic.projectFromEuclideanSpace
                Hyperbolic.projectFromEuclideanSpace
            )
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


euclideanProjectionOfLines : Html msg
euclideanProjectionOfLines =
    grid
        |> List.map
            (Tuple.mapBoth Hyperbolic.projectFromEuclideanSpace
                Hyperbolic.projectFromEuclideanSpace
            )
        |> List.filterMap Hyperbolic.lineFromLineSegment
        |> List.concatMap (Hyperbolic.pointsAlongLine 1000)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


beltramiLines : Html msg
beltramiLines =
    grid
        |> List.map
            (Tuple.mapBoth Hyperbolic.projectFromEuclideanSpace
                Hyperbolic.projectFromEuclideanSpace
            )
        |> List.filterMap Hyperbolic.lineFromLineSegment
        |> List.concatMap (Hyperbolic.pointsAlongLine 1000)
        |> List.map Hyperbolic.projectOntoBeltramiKleinDisc
        |> View.Canvas.hyperbolic


rosette : List ( Float, Float )
rosette =
    let
        pointsPerCircle =
            1000

        circleSize =
            1

        circleAround ( x, y ) =
            List.range 0 (pointsPerCircle - 1)
                |> List.map (\i -> toFloat i / pointsPerCircle)
                |> List.map (\amount -> fromPolar ( circleSize, amount * 2 * pi ))
                |> List.map (Tuple.mapBoth ((+) x) ((+) y))
    in
    List.range 0 5
        |> List.map (\i -> toFloat i / 6)
        |> List.map (\amount -> fromPolar ( circleSize, amount * 2 * pi ))
        |> List.concatMap circleAround


euclideanRosette : Html msg
euclideanRosette =
    [ rosette
        |> View.Canvas.euclidean
    , rosette
        |> List.map Hyperbolic.projectFromEuclideanSpace
        |> List.map Hyperbolic.projectOntoBeltramiKleinDisc
        |> View.Canvas.euclidean
    , rosette
        |> List.map Hyperbolic.projectFromEuclideanSpace
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.euclidean
    ]
        |> Layout.row Layout.centered


chapter : Chapter (Html msg)
chapter =
    ElmBook.Chapter.chapter "Models of the Hyperbolic Plane"
        |> ElmBook.Chapter.withComponentList
            [ ( "Coordinates in Euclidean Space", euclideanCoords )
            , ( "Projecting euclidean points into Hyperbolic Space", euclideanProjection )
            , ( "Projecting euclidean lines into Hyperbolic space", euclideanProjectionOfLines )
            , ( "Projecting euclidean lines onto the Beltrami-Klein Disc", beltramiLines )
            , ( "Six petal rosette in different models", euclideanRosette )
            ]
        |> ElmBook.Chapter.render """
Let's try to get an image of the hyperbolic plane onto our screen.

## Projection from euclidean space

Our most intuitive approach would be to introduce a coordinate system.

```
grid : List ((Float, Float), (Float, Float))
grid =
    List.range 0 20
        |> List.map (\\i -> toFloat i / 20)
        |> List.concatMap
            (\\ratio ->
                [ ( ( -1, ratio * 2 - 1 )
                  , ( 1, ratio * 2 - 1 )
                  )
                , ( ( ratio * 2 - 1, -1 )
                  , ( ratio * 2 - 1, 1 )
                  )
                ]
            )
```

<component with-label="Coordinates in Euclidean Space" />

We could now project the points onto a model of the hyperbolic space. For now we will use the Poincaré Hyperbolic Disc Model.

```
let
    pointsPerLine =
        100
in
grid
    |> List.map
        (Tuple.mapBoth 
            Hyperbolic.projectFromEuclideanSpace
            Hyperbolic.projectFromEuclideanSpace
        )
    |> List.concatMap 
        (Hyperbolic.pointsAlongLineSegment pointsPerLine)
    |> List.map Hyperbolic.projectOntoPoincareDisc
```

<component with-label="Projecting euclidean points into Hyperbolic Space" />

We might notice a few things:

1. A gray circle appeared around our image
2. The image got smaller
3. Lines are bend inwards
4. The endpoints of the lines are stretched outwards
4. Our original squares seemingly got destroyed (They are just line segments, that don't meet).

These are some of the most interesting characteristics of hyperbolic space and it will take some time until I can explain the reason behind all of this points to you.

But for now lets start with the gray circle. This gray circle is actually infinity. 
In most representations of the hyperbolic space, we project the space onto a shape, such that infinity is on its edge. 
In fact you can actually create geometric shapes at infinity. However in hyperbolic space infinity is not just one point - its all the points on the gray circle.

Thats why from now on out we will say **ideal point** if we mean, that the point lies at infinity (on the gray circle).

So lets try again but this time, we extend our line segments out to infinity.

```
grid
    |> List.map
        (Tuple.mapBoth Hyperbolic.projectFromEuclideanSpace
            Hyperbolic.projectFromEuclideanSpace
        )
    --convert line segments into lines
    |> List.filterMap Hyperbolic.lineFromLineSegment 
    |> List.concatMap (Hyperbolic.pointsAlongLine 1000)
    |> List.map Hyperbolic.projectOntoPoincareDisc
```

<component with-label="Projecting euclidean lines into Hyperbolic space" />

We can see very nicely how the lines are bend inwards. We can actually fix this by using the Beltrami-Klein Model instead.

```
    grid
        |> List.map
            (Tuple.mapBoth Hyperbolic.projectFromEuclideanSpace
                Hyperbolic.projectFromEuclideanSpace
            )
        |> List.filterMap Hyperbolic.lineFromLineSegment
        |> List.concatMap (Hyperbolic.pointsAlongLine 1000)
        --project onto the Beltrami-Klein Disc
        |> List.map Hyperbolic.projectOntoBeltramiKleinDisc
        |> hyperbolicToCanvas
```

<component with-label="Projecting euclidean lines onto the Beltrami-Klein Disc" />

So we have now seen two different model of hyperbolic space:

* In the Poincaré Hyperbolic Disc the lines became arcs and 
* in the Beltrami-Klein Disc the lines stayed euclidean lines

There are also a couple of other models and ways [how to construct a coordinate system in the hyperbolic space](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates).
However, we will only look at these two representation, as they are the most useful.

## Comparing the Poincaré Hyperbolic Disc Model with the Beltrami-Klein Disc Model

To get a better understanding of the difference between these two models, we will construct a six petal rosette and compare the results.

```
rosette : List ( Float, Float )
rosette =
    let
        pointsPerCircle =
            1000

        circleSize =
            1

        circleAround ( x, y ) =
            List.range 0 (pointsPerCircle - 1)
                |> List.map (\\i -> toFloat i / pointsPerCircle)
                |> List.map (\\amount -> fromPolar ( circleSize, amount * 2 * pi ))
                |> List.map (Tuple.mapBoth ((+) x) ((+) y))
    in
    List.range 0 5
        |> List.map (\\i -> toFloat i / 6)
        |> List.map (\\amount -> fromPolar ( circleSize, amount * 2 * pi ))
        |> List.concatMap circleAround
```

<component with-label="Six petal rosette in different models" />

The first image is in euclidean space. But the second image is actually in the Beltrami-Klein Disc! Notice how the circles got distorted.
Now compare this with the third image, which is in the Poincaré Hyperbolic Disc. Here the circles are still a bit distorted but more or less intact.

Sadly this demonstration is not perfect, as we have constructed euclidean circles and not hyperbolic ones.
Hyperbolic circles would not have any visible distortion in the Poincaré Hyperbolic Disc.
But we still have a lot to learn before we can construct hyperbolic circles.

However, you should now have a basic understanding of the two models:

* The **Beltrami-Klein Disc** tries to keep lines intact and will distort everything else, 
* The **Poincaré Hyperbolic** Disc tries to keep circles intact and distorts the rest.

Because of that, its easier to do compass and straightedge constructions in the Beltrami-Klein Disc, but easier to do circles and angles in the Poincaré Hyperbolic Disc.

Fortunately, it's quite simple to convert between the two. Going forward, we will use the Poincaré Hyperbolic Disc for visualization and the Beltrami-Klein Disc as the internal model.
The conversion will happen behind the scenes, so you don't have to worry.
"""
