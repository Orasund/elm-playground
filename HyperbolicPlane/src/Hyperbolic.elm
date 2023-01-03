module Hyperbolic exposing (..)

import Internal exposing (artanh, normalVector, tanh)


{-| Module for working with hyperbolic geometry. Uses the Beltrami–Klein model.

Ideal points have Beltrami coordinates with length 1.

Hyper ideal points have Beltrami coordinates with length > 1.

-}
type BeltramiCoords
    = BeltramiCoords ( Float, Float )


type AxialCoords
    = AxialCoords ( Float, Float )


{-| An [ideal point](https://en.wikipedia.org/wiki/Ideal_point) is a point at infinity and can be represented by an angle
-}
type IdealPoint
    = IdealPoint Float


{-| A hyper ideal point is a point that only exists in the euclidean projection of the hyperbolic plane into the Beltrami–Klein model.
-}
type HyperIdealPoint
    = HyperIdealPoint ( Float, Float )


{-| We define a line by two [ideal points](https://en.wikipedia.org/wiki/Ideal_point).
-}
type alias Line =
    ( IdealPoint, IdealPoint )


type alias LineSegment =
    ( BeltramiCoords, BeltramiCoords )


pointAtInfinity : Float -> IdealPoint
pointAtInfinity angle =
    IdealPoint angle


origin : BeltramiCoords
origin =
    BeltramiCoords ( 0, 0 )


pointsAreEqual : BeltramiCoords -> BeltramiCoords -> Bool
pointsAreEqual (BeltramiCoords ( x1, y1 )) (BeltramiCoords ( x2, y2 )) =
    Internal.equal x1 x2 && Internal.equal y1 y2


{-| Beltrami-Klein distance function
-}
distanceTo : BeltramiCoords -> BeltramiCoords -> Float
distanceTo (BeltramiCoords u) (BeltramiCoords v) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    let
        ( tempA, tempB ) =
            Internal.lineToGeneralForm ( u, v )
                |> Internal.intersectLineWithUnitCircle

        ( a, b ) =
            if Internal.distance tempA u < Internal.distance tempA u then
                ( tempA, tempB )

            else
                ( tempB, tempA )
    in
    logBase e
        ((Internal.distance v a * Internal.distance b u)
            / (Internal.distance u a * Internal.distance b v)
        )
        / 2



------------------------------------------------------------------------------------------
-- Line And Line Section
------------------------------------------------------------------------------------------


lineFromIdealPoints : IdealPoint -> IdealPoint -> Line
lineFromIdealPoints =
    Tuple.pair


lineSectionTo : BeltramiCoords -> BeltramiCoords -> LineSegment
lineSectionTo =
    Tuple.pair


lineFromPoints : BeltramiCoords -> BeltramiCoords -> Line
lineFromPoints (BeltramiCoords b1) (BeltramiCoords b2) =
    let
        ( ( _, i1 ), ( _, i2 ) ) =
            Internal.lineToGeneralForm ( b1, b2 )
                |> Internal.intersectLineWithUnitCircle
                |> Tuple.mapBoth toPolar toPolar
    in
    lineFromIdealPoints (IdealPoint i1) (IdealPoint i2)


{-| Any line except lines going through the origin, can be associated by a unique hyper ideal point.
-}
poleOfLine : Line -> Maybe HyperIdealPoint
poleOfLine ( i1, i2 ) =
    let
        (BeltramiCoords p1) =
            fromIdealPoint i1

        (BeltramiCoords p2) =
            fromIdealPoint i2

        v1 =
            normalVector ( p1, ( 0, 0 ) )

        v2 =
            normalVector ( p2, ( 0, 0 ) )
    in
    Internal.lineIntersection ( p1, Internal.plus p1 v1 ) ( p2, Internal.plus p2 v2 )
        |> Maybe.map HyperIdealPoint


{-| Point as to lie on the line
-}
perpendicularLineThrough : BeltramiCoords -> Line -> Line
perpendicularLineThrough (BeltramiCoords p) line =
    case poleOfLine line of
        Just (HyperIdealPoint pole) ->
            lineFromPoints (BeltramiCoords pole) (BeltramiCoords p)

        Nothing ->
            --line through zero
            let
                ( BeltramiCoords p1, BeltramiCoords p2 ) =
                    line
                        |> Tuple.mapBoth fromIdealPoint fromIdealPoint
            in
            lineFromPoints
                (Internal.normalVector ( p1, p2 )
                    |> Internal.plus p
                    |> BeltramiCoords
                )
                (BeltramiCoords p)


{-| Two lines may intersect at exactly one point
-}
intersectLines : Line -> Line -> Maybe BeltramiCoords
intersectLines l1 l2 =
    let
        ( BeltramiCoords p1, BeltramiCoords p2 ) =
            l1 |> Tuple.mapBoth fromIdealPoint fromIdealPoint

        ( BeltramiCoords p3, BeltramiCoords p4 ) =
            l2 |> Tuple.mapBoth fromIdealPoint fromIdealPoint
    in
    Internal.lineIntersection ( p1, p2 ) ( p3, p4 )
        |> Maybe.map BeltramiCoords


pointsAlongLineSegment : Int -> LineSegment -> List BeltramiCoords
pointsAlongLineSegment n ( BeltramiCoords ( x1, y1 ), BeltramiCoords ( x2, y2 ) ) =
    let
        vecX =
            x2 - x1

        vecY =
            y2 - y1
    in
    List.range 0 n
        |> List.map (\i -> toFloat i / toFloat n)
        |> List.map (\amount -> ( x1 + amount * vecX, y1 + amount * vecY ))
        |> List.map BeltramiCoords



------------------------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------------------------


fromIdealPoint : IdealPoint -> BeltramiCoords
fromIdealPoint (IdealPoint angle1) =
    ( 1, angle1 ) |> fromPolar |> BeltramiCoords


{-| -}
fromPolarCoords : { radius : Float, angle : Float } -> BeltramiCoords
fromPolarCoords args =
    --https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane
    Tuple.pair
        (tanh args.radius * cos args.angle)
        (tanh args.radius * sin args.angle)
        |> BeltramiCoords


{-| Projects points from euclidean space into hyperbolic space.
However, you can not expect the proportions to stay the same.

Points further out experience more distortion.

-}
projectFromEuclideanSpace : ( Float, Float ) -> BeltramiCoords
projectFromEuclideanSpace ( x, y ) =
    toPolar ( x, y )
        |> (\( radius, angle ) -> fromPolarCoords { radius = radius, angle = angle })


{-| Converts Beltrami Coordinates into [Axial coordinates](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates).
-}
fromAxialCoords : AxialCoords -> BeltramiCoords
fromAxialCoords (AxialCoords ( x, y )) =
    BeltramiCoords ( tanh x, tanh y )


{-| Converts [Axial coordinates](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates) into Beltrami Coordinates.
-}
toAxialCoords : BeltramiCoords -> Maybe AxialCoords
toAxialCoords (BeltramiCoords ( x, y )) =
    Maybe.map2 Tuple.pair
        (artanh x)
        (artanh y)
        |> Maybe.map AxialCoords


toBeltramiCoordinates : BeltramiCoords -> ( Float, Float )
toBeltramiCoordinates (BeltramiCoords p) =
    p


toPoincareCoordinates : BeltramiCoords -> ( Float, Float )
toPoincareCoordinates (BeltramiCoords p) =
    p
        |> (\( x, y ) ->
                let
                    length =
                        1 + sqrt (1 - x * x - y * y)
                in
                ( x / length
                , y / length
                )
           )
