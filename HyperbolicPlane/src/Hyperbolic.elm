module Hyperbolic exposing (..)

import Html exposing (u)
import Internal exposing (artanh, normalVector, tanh)


{-| Module for working with hyperbolic geometry. Uses the Beltrami–Klein model.

Ideal points have Beltrami coordinates with length 1.

Hyper ideal points have Beltrami coordinates with length > 1.

-}
type BeltramiCoord
    = BeltramiCoord ( Float, Float )


type AxialCoord
    = AxialCoord ( Float, Float )


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
    ( BeltramiCoord, BeltramiCoord )


type alias Vector =
    { values : BeltramiCoord }


{-| Gyrovector in Poincare-Model
-}
type PoincareVector
    = PoincareVector ( Float, Float )


pointAtInfinity : Float -> IdealPoint
pointAtInfinity angle =
    IdealPoint angle


origin : BeltramiCoord
origin =
    BeltramiCoord ( 0, 0 )


pointsAreEqual : BeltramiCoord -> BeltramiCoord -> Bool
pointsAreEqual (BeltramiCoord ( x1, y1 )) (BeltramiCoord ( x2, y2 )) =
    Internal.equal x1 x2 && Internal.equal y1 y2


{-| Beltrami-Klein distance function
-}
distanceTo : BeltramiCoord -> BeltramiCoord -> Float
distanceTo (BeltramiCoord u) (BeltramiCoord v) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    case
        Internal.lineToGeneralForm ( u, v )
            |> Internal.intersectLineWithUnitCircle
    of
        Just ( tempA, tempB ) ->
            let
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

        Nothing ->
            0


distanceToOrigin : BeltramiCoord -> Float
distanceToOrigin =
    distanceTo origin


{-| If you want to construct a tiling in the hyperbolic space, then the length of the polygon specifies how many polygons fit around a point.

If you look up tilings, you will usually find a Schläfli symbol {p,q}. In that case p = vertices, q = polygonsAroundAPoint.

The resulting length is for BeltramiCoords

-}
discFillingPolygon : { vertices : Int, polygonsAroundAPoint : Int } -> Float
discFillingPolygon args =
    --https://math.stackexchange.com/questions/1331199/edge-length-of-hyperbolic-tesselations
    (cos (pi / toFloat args.vertices)
        / sin (pi / toFloat args.polygonsAroundAPoint)
    )
        |> Internal.arcosh
        |> (*) 2



------------------------------------------------------------------------------------------
-- Line And Line Section
------------------------------------------------------------------------------------------


{-| Constructs a line from two ideal points

    lineFromIdealPoints : IdealPoint -> IdealPoint -> Line
    lineFromIdealPoints =
        Tuple.pair

-}
lineFromIdealPoints : IdealPoint -> IdealPoint -> Line
lineFromIdealPoints =
    Tuple.pair


lineSectionTo : BeltramiCoord -> BeltramiCoord -> LineSegment
lineSectionTo =
    Tuple.pair


lineFromPoints : BeltramiCoord -> BeltramiCoord -> Maybe Line
lineFromPoints (BeltramiCoord b1) (BeltramiCoord b2) =
    Internal.lineToGeneralForm ( b1, b2 )
        |> Internal.intersectLineWithUnitCircle
        |> Maybe.map (Tuple.mapBoth toPolar toPolar)
        |> Maybe.map
            (\( ( _, i1 ), ( _, i2 ) ) ->
                lineFromIdealPoints (IdealPoint i1) (IdealPoint i2)
            )


lineFromLineSegment : LineSegment -> Maybe Line
lineFromLineSegment ( p1, p2 ) =
    lineFromPoints p1 p2


lineFromHyperIdealPointThrough : BeltramiCoord -> HyperIdealPoint -> Maybe Line
lineFromHyperIdealPointThrough p (HyperIdealPoint h) =
    lineFromPoints p (BeltramiCoord h)


{-| Any line except lines going through the origin, can be associated by a unique hyper ideal point.
-}
poleOfLine : Line -> Maybe HyperIdealPoint
poleOfLine ( i1, i2 ) =
    let
        (BeltramiCoord p1) =
            unsafeFromIdealPoint i1

        (BeltramiCoord p2) =
            unsafeFromIdealPoint i2

        v1 =
            normalVector ( p1, ( 0, 0 ) )

        v2 =
            normalVector ( p2, ( 0, 0 ) )
    in
    Internal.lineToGeneralForm ( p2, Internal.plus p2 v2 )
        |> Internal.lineIntersection (Internal.lineToGeneralForm ( p1, Internal.plus p1 v1 ))
        |> Maybe.map HyperIdealPoint


{-| reflect a point by a line
-}
reflectBy : Line -> BeltramiCoord -> BeltramiCoord
reflectBy line p0 =
    perpendicularLineThrough p0 line
        |> Maybe.andThen (intersectLines line)
        |> Maybe.map
            (\p1 ->
                toPoincareVector p1
                    |> add (vectorTo (toPoincareVector p0) (toPoincareVector p1))
                    |> fromPoincareVector
            )
        |> Maybe.withDefault p0


{-| Point as to lie on the line
-}
perpendicularLineThrough : BeltramiCoord -> Line -> Maybe Line
perpendicularLineThrough (BeltramiCoord p) line =
    case poleOfLine line of
        Just (HyperIdealPoint pole) ->
            lineFromPoints (BeltramiCoord pole) (BeltramiCoord p)

        Nothing ->
            --line through origin
            let
                ( BeltramiCoord p1, BeltramiCoord p2 ) =
                    line
                        |> Tuple.mapBoth unsafeFromIdealPoint unsafeFromIdealPoint
            in
            lineFromPoints
                (Internal.normalVector ( p1, p2 )
                    |> Internal.plus p
                    |> BeltramiCoord
                )
                (BeltramiCoord p)


nearestIdealPointOf : Line -> BeltramiCoord -> IdealPoint
nearestIdealPointOf ( i1, i2 ) (BeltramiCoord p) =
    let
        (BeltramiCoord p1) =
            unsafeFromIdealPoint i1

        (BeltramiCoord p2) =
            unsafeFromIdealPoint i2
    in
    if Internal.distance p1 p < Internal.distance p2 p then
        i1

    else
        i2


bisectorThrough : BeltramiCoord -> ( BeltramiCoord, BeltramiCoord ) -> Maybe Line
bisectorThrough c ( a, b ) =
    Maybe.map2
        (\i1 i2 ->
            poleOfLine ( i1, i2 )
        )
        (lineFromPoints c a
            |> Maybe.map (\line -> nearestIdealPointOf line a)
        )
        (lineFromPoints c b
            |> Maybe.map (\line -> nearestIdealPointOf line b)
        )
        |> Maybe.andThen identity
        |> Maybe.andThen (lineFromHyperIdealPointThrough c)


midpointOfLine : Line -> BeltramiCoord
midpointOfLine ( i1, i2 ) =
    let
        (BeltramiCoord ( x1, y1 )) =
            unsafeFromIdealPoint i1

        (BeltramiCoord ( x2, y2 )) =
            unsafeFromIdealPoint i2
    in
    ( (x1 - x2) / 2 + x2, (y1 - y2) / 2 + y2 )
        |> BeltramiCoord


midpointOfLineSegment : LineSegment -> Maybe BeltramiCoord
midpointOfLineSegment ( p1, p2 ) =
    lineFromPoints p1 p2
        |> Maybe.andThen
            (\line ->
                Maybe.map2
                    (\( i1, _ ) ( _, i4 ) ->
                        intersectLines ( i1, i4 ) line
                    )
                    (perpendicularLineThrough p1 line)
                    (perpendicularLineThrough p2 line)
                    |> Maybe.andThen identity
            )


{-| Two lines may intersect at exactly one point
-}
intersectLines : Line -> Line -> Maybe BeltramiCoord
intersectLines l1 l2 =
    let
        ( BeltramiCoord p1, BeltramiCoord p2 ) =
            l1 |> Tuple.mapBoth unsafeFromIdealPoint unsafeFromIdealPoint

        ( BeltramiCoord p3, BeltramiCoord p4 ) =
            l2 |> Tuple.mapBoth unsafeFromIdealPoint unsafeFromIdealPoint
    in
    Internal.lineIntersection (Internal.lineToGeneralForm ( p1, p2 ) |> Debug.log "g1") (Internal.lineToGeneralForm ( p3, p4 ) |> Debug.log "g2")
        |> Maybe.map BeltramiCoord


pointsAlongLineSegment : Int -> LineSegment -> List BeltramiCoord
pointsAlongLineSegment n ( BeltramiCoord ( x1, y1 ), BeltramiCoord ( x2, y2 ) ) =
    let
        vecX =
            x2 - x1

        vecY =
            y2 - y1
    in
    List.range 0 n
        |> List.map (\i -> toFloat i / toFloat n)
        |> List.map (\amount -> ( x1 + amount * vecX, y1 + amount * vecY ))
        |> List.map BeltramiCoord


pointsAlongLine : Int -> Line -> List BeltramiCoord
pointsAlongLine n ( i1, i2 ) =
    pointsAlongLineSegment n ( unsafeFromIdealPoint i1, unsafeFromIdealPoint i2 )



------------------------------------------------------------------------------------------
-- Vectors
------------------------------------------------------------------------------------------


{-| creates a vector from two points. The length of the vector
-}
vectorTo : PoincareVector -> PoincareVector -> PoincareVector
vectorTo p2 p1 =
    p2 |> negate |> add p1


scaleBy : Float -> PoincareVector -> PoincareVector
scaleBy a (PoincareVector x) =
    --https://andbloch.github.io/K-Stereographic-Model/
    --kappa = -1
    Internal.artanh (Internal.length x)
        |> Maybe.map ((*) a)
        |> Maybe.map Internal.tanh
        |> Maybe.map (\v -> v / Internal.length x)
        |> Maybe.map (\v -> Internal.scaleBy v x)
        |> Maybe.withDefault x
        |> PoincareVector


rotateClockwise : Float -> PoincareVector -> PoincareVector
rotateClockwise amount (PoincareVector ( x, y )) =
    {--toPolar v
        |> Tuple.mapSecond ((+) amount)
        |> fromPolar
        |> PoincareVector--}
    ( x * cos amount - y * sin amount
    , x * sin amount + y * cos amount
    )
        |> PoincareVector


rotatePointClockwise : Float -> BeltramiCoord -> BeltramiCoord
rotatePointClockwise amount (BeltramiCoord ( x, y )) =
    --https://en.wikipedia.org/wiki/Rotation_matrix#:~:text=To%20perform%20the%20rotation%20on,the%20trigonometric%20summation%20angle%20formulae.
    ( x * cos amount - y * sin amount
    , x * sin amount + y * cos amount
    )
        |> BeltramiCoord


setRotation : Float -> BeltramiCoord -> BeltramiCoord
setRotation amount (BeltramiCoord v) =
    toPolar v
        |> Tuple.mapSecond (\_ -> amount)
        |> fromPolar
        |> BeltramiCoord


rotateClockwiseAround : PoincareVector -> Float -> PoincareVector -> PoincareVector
rotateClockwiseAround p amount p0 =
    add
        (p
            |> vectorTo p0
            |> rotateClockwise amount
        )
        p


negate : PoincareVector -> PoincareVector
negate (PoincareVector v) =
    v
        |> Internal.negate
        |> PoincareVector


einsteinVelocityAddition : BeltramiCoord -> BeltramiCoord -> BeltramiCoord
einsteinVelocityAddition (BeltramiCoord b) (BeltramiCoord a) =
    --https://en.wikipedia.org/wiki/Gyrovector_space
    let
        v =
            b

        u =
            a

        cSquared =
            1

        gamma =
            1 / sqrt (1 - Internal.innerProduct u u / cSquared)
    in
    u
        |> Internal.plus (v |> Internal.scaleBy (1 / gamma))
        |> Internal.plus (u |> Internal.scaleBy (gamma * Internal.innerProduct u v / (cSquared * (1 + gamma))))
        |> Internal.scaleBy (1 / (1 + Internal.innerProduct u v / cSquared))
        |> BeltramiCoord



{--Internal.plus a b
        |> Internal.scaleBy (1 / (1 + product))
        |> Internal.plus
            ((a
                |> Internal.scaleBy product
                |> Internal.minus
                    (b
                        |> Internal.scaleBy lengthSquared
                    )
             )
                |> Internal.scaleBy (1 / ((1 + norm) * (1 + product)))
            )
        |> BeltramiCoord
        --}


{-| Using the Möbius Addition to add two vectors in the poincare disc
-}
add : PoincareVector -> PoincareVector -> PoincareVector
add (PoincareVector v) (PoincareVector u) =
    --https://en.wikipedia.org/wiki/Gyrovector_space#Beltrami%E2%80%93Klein_disc/ball_model_and_Einstein_addition
    --s = 1
    let
        normSquared x =
            Internal.innerProduct x x
    in
    u
        |> Internal.scaleBy
            (1
                + (2 * Internal.innerProduct u v)
                + Internal.innerProduct v v
            )
        |> Internal.plus (v |> Internal.scaleBy (1 - normSquared u))
        |> Internal.scaleBy
            (1
                / (1
                    + (2 * Internal.innerProduct u v)
                    + (normSquared v * normSquared u)
                  )
            )
        |> PoincareVector


translateBy : PoincareVector -> BeltramiCoord -> BeltramiCoord
translateBy v p =
    p
        |> toPoincareVector
        |> add v
        |> fromPoincareVector


length : PoincareVector -> Float
length (PoincareVector v) =
    v
        |> Internal.length
        |> Internal.artanh
        |> Maybe.withDefault 0
        |> (*) 2



------------------------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------------------------


{-| -}
fromPolarCoords : { radius : Float, angle : Float } -> BeltramiCoord
fromPolarCoords args =
    --https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane
    Tuple.pair
        (tanh args.radius * cos args.angle)
        (tanh args.radius * sin args.angle)
        |> BeltramiCoord


{-| Projects points from euclidean space into hyperbolic space.
However, you can not expect the proportions to stay the same.

Points further out experience more distortion.

-}
projectFromEuclideanSpace : ( Float, Float ) -> BeltramiCoord
projectFromEuclideanSpace ( x, y ) =
    toPolar ( x, y )
        |> (\( radius, angle ) -> fromPolarCoords { radius = radius, angle = angle })


{-| Converts Beltrami Coordinates into [Axial coordinates](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates).
-}
unsafeFromAxialCoord : ( Float, Float ) -> BeltramiCoord
unsafeFromAxialCoord ( x, y ) =
    BeltramiCoord ( tanh x, tanh y )


{-| Converts [Axial coordinates](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates) into Beltrami Coordinates.
-}
unsafeToAxialCoord : BeltramiCoord -> Maybe ( Float, Float )
unsafeToAxialCoord (BeltramiCoord ( x, y )) =
    Maybe.map2 Tuple.pair
        (artanh x)
        (artanh y)


projectOntoBeltramiKleinDisc : BeltramiCoord -> ( Float, Float )
projectOntoBeltramiKleinDisc (BeltramiCoord p) =
    p


toPoincareVector : BeltramiCoord -> PoincareVector
toPoincareVector (BeltramiCoord s) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    let
        l =
            1 + sqrt (1 - Internal.innerProduct s s)
    in
    s
        |> Internal.scaleBy (1 / l)
        |> PoincareVector


fromPoincareVector : PoincareVector -> BeltramiCoord
fromPoincareVector (PoincareVector u) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    u
        |> Internal.scaleBy (2 / (1 + Internal.innerProduct u u))
        |> BeltramiCoord


projectOntoPoincareDisc : BeltramiCoord -> ( Float, Float )
projectOntoPoincareDisc p =
    p
        |> toPoincareVector
        |> (\(PoincareVector s) -> s)


unsafeHyperIdealPointToRecord : HyperIdealPoint -> { x : Float, y : Float }
unsafeHyperIdealPointToRecord (HyperIdealPoint ( x, y )) =
    { x = x, y = y }


unsafeFromIdealPoint : IdealPoint -> BeltramiCoord
unsafeFromIdealPoint (IdealPoint angle1) =
    ( 1, angle1 ) |> fromPolar |> BeltramiCoord


unsafeFromRecord : { x : Float, y : Float } -> BeltramiCoord
unsafeFromRecord { x, y } =
    BeltramiCoord ( x, y )


unsafePoincareVectorFromRecord : { x : Float, y : Float } -> PoincareVector
unsafePoincareVectorFromRecord { x, y } =
    PoincareVector ( x, y )


unsafeToRecord : BeltramiCoord -> { x : Float, y : Float }
unsafeToRecord (BeltramiCoord ( x, y )) =
    { x = x, y = y }


unsafePoincareVectorToRecord : PoincareVector -> { x : Float, y : Float }
unsafePoincareVectorToRecord (PoincareVector ( x, y )) =
    { x = x, y = y }
