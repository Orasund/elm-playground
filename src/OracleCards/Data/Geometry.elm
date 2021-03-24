module OracleCards.Data.Geometry exposing (innerTangent, intersection, outerTangent)

import Circle2d exposing (Circle2d)
import Direction2d
import Point2d exposing (Point2d)
import Quantity
import Vector2d


{-| <https://mathworld.wolfram.com/Circle-CircleIntersection.html#:~:text=The%20intersections%20of%20two%20circles,known%20as%20the%20radical%20center>.
-}
intersection : ( Circle2d Float (), Circle2d Float () ) -> ( Point2d Float (), Point2d Float () )
intersection ( c1, c2 ) =
    let
        p1 =
            c1 |> Circle2d.centerPoint

        p2 =
            c2 |> Circle2d.centerPoint

        d12 =
            Point2d.distanceFrom p1 p2

        dir12 =
            Direction2d.from p1 p2
                |> Maybe.withDefault Direction2d.positiveX

        r1 =
            c1 |> Circle2d.radius

        r2 =
            c2 |> Circle2d.radius

        d1 =
            ((d12 |> Quantity.unwrap)
                * (d12 |> Quantity.unwrap)
                - ((r2 |> Quantity.unwrap) * (r2 |> Quantity.unwrap))
                + ((r1 |> Quantity.unwrap) * (r1 |> Quantity.unwrap))
            )
                / ((d12 |> Quantity.unwrap) * 2)
                |> Quantity.unsafe

        h =
            ((r1 |> Quantity.unwrap) * (r1 |> Quantity.unwrap) - (d1 |> Quantity.unwrap) * (d1 |> Quantity.unwrap))
                |> sqrt
                |> Quantity.unsafe

        x1 =
            p1
                |> Point2d.translateBy (Vector2d.withLength d1 dir12)
                |> Point2d.translateBy (Vector2d.withLength h (dir12 |> Direction2d.rotateClockwise))

        x2 =
            p1
                |> Point2d.translateBy (Vector2d.withLength d1 dir12)
                |> Point2d.translateBy (Vector2d.withLength h (dir12 |> Direction2d.rotateCounterclockwise))
    in
    ( x1, x2 )


{-|

    1. Set M1 = center of bigger circle, M2 = smaller one
    2. Construct Circle k1 with center M1 and radius (r1 - r2)
    3. Set M3 = center of M1, M2
    4. Set x = crosspoint of circles M1 with radius r1+r2/2 and M3 with radius r2
    5. Set d = direction from M1 to x
    6. return points on circle of M1 M2 with direction d

    https://mmf.univie.ac.at/fileadmin/user_upload/p_mathematikmachtfreunde/Materialien/KB-Gemeinsame_Tangenten_zweier_Kreise-Ausarbeitung.pdf

-}
outerTangent : ( Circle2d Float (), Circle2d Float () ) -> Bool -> ( Point2d Float (), Point2d Float () )
outerTangent ( c1, c2 ) isNextClockwise =
    if c1 |> Circle2d.radius |> Quantity.lessThan (c2 |> Circle2d.radius) then
        outerTangent ( c2, c1 ) (not isNextClockwise)
            |> (\( p2, p1 ) -> ( p1, p2 ))

    else
        let
            p1 =
                c1 |> Circle2d.centerPoint

            p2 =
                c2 |> Circle2d.centerPoint

            r1 =
                c1 |> Circle2d.radius

            r2 =
                c2 |> Circle2d.radius

            c3 =
                Circle2d.withRadius ((r1 |> Quantity.unwrap) - (r2 |> Quantity.unwrap) |> Quantity.unsafe) p1

            d12 =
                Point2d.distanceFrom p1 p2

            c4 =
                --checked d12 /2 is correct
                Circle2d.withRadius (d12 |> Quantity.divideBy 2)
                    (Point2d.midpoint p1 p2)

            ( x1, x2 ) =
                intersection ( c3, c4 )

            d =
                if isNextClockwise then
                    Direction2d.from p1 x1
                        |> Maybe.withDefault Direction2d.positiveX

                else
                    Direction2d.from p1 x2
                        |> Maybe.withDefault Direction2d.positiveX
        in
        ( p1 |> Point2d.translateBy (Vector2d.withLength r1 d)
        , p2 |> Point2d.translateBy (Vector2d.withLength r2 d)
        )


{-|

    1. Set M1 = center of bigger circle, M2 = smaller one
    2. Construct Circle k3 with center M1 and radius (r1 + r2)
    3. Set M3 = center of M1, M2
    4. Set x1 = crosspoint of circles k3  and M3 with radius M1->M2/2
    5. Set d = direction from M1 to x
    6. return points on circle of M1 with direction d and M2 with direction -d

    https://mmf.univie.ac.at/fileadmin/user_upload/p_mathematikmachtfreunde/Materialien/KB-Gemeinsame_Tangenten_zweier_Kreise-Ausarbeitung.pdf

-}
innerTangent : ( Circle2d Float (), Circle2d Float () ) -> Bool -> ( Point2d Float (), Point2d Float () )
innerTangent ( c1, c2 ) isNextClockwise =
    if c1 |> Circle2d.radius |> Quantity.lessThan (c2 |> Circle2d.radius) then
        innerTangent ( c2, c1 ) isNextClockwise
            --(not isNextClockwise)
            |> (\( p2, p1 ) -> ( p1, p2 ))

    else
        let
            p1 =
                c1 |> Circle2d.centerPoint

            p2 =
                c2 |> Circle2d.centerPoint

            r1 =
                c1 |> Circle2d.radius

            r2 =
                c2 |> Circle2d.radius

            c3 =
                Circle2d.withRadius (r1 |> Quantity.plus r2) p1

            d12 =
                Point2d.distanceFrom p1 p2

            c4 =
                Circle2d.withRadius (d12 |> Quantity.divideBy 2)
                    (Point2d.midpoint p1 p2)

            ( x1, x2 ) =
                intersection ( c3, c4 )

            d =
                if isNextClockwise then
                    Direction2d.from p1 x2
                        |> Maybe.withDefault Direction2d.positiveX

                else
                    Direction2d.from p1 x1
                        |> Maybe.withDefault Direction2d.positiveX
        in
        ( p1 |> Point2d.translateBy (Vector2d.withLength r1 d)
        , p2 |> Point2d.translateBy (Vector2d.withLength r2 (d |> Direction2d.reverse))
        )
