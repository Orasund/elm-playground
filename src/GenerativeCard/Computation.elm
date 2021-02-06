module GenerativeCard.Computation exposing (Computation(..), step)

import Angle
import Axis2d exposing (Axis2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Random exposing (Generator)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type alias Data =
    { from : Point2d Pixels ()
    , currentPath : List (Point2d Pixels ())
    , previousPaths : List (List (Point2d Pixels ()))
    , iter : Int
    , searchList : List ( Point2d Pixels (), Point2d Pixels () )
    , drawings : List ( Point2d Pixels (), Point2d Pixels () )
    }


type Computation
    = Running
        { remaining : List (List (Point2d Pixels ()))
        , result : Data
        , lastUpdate : Maybe Posix
        , chunkSize : Int
        }
    | Done Data


smallesLineSize : Float
smallesLineSize =
    10


{-| 0: always creates tiangle with biggest area
1 : triangle of any shape
-}
spread : Float
spread =
    0.1


findSmallestDistanceTo :
    { maxDistance : Float, target : Point2d Pixels (), targetAxis : Axis2d Pixels () }
    -> List ( Point2d Pixels (), Point2d Pixels () )
    -> ( Float, Float )
findSmallestDistanceTo { maxDistance, target, targetAxis } list =
    case list of
        [] ->
            ( maxDistance, maxDistance )

        _ ->
            list
                |> List.foldl
                    (\( from, to ) dist ->
                        Axis2d.throughPoints from to
                            |> Maybe.map
                                (\axis ->
                                    let
                                        distanceAlong =
                                            target
                                                |> Point2d.signedDistanceAlong axis
                                                |> Pixels.toFloat

                                        bestApprox =
                                            if distanceAlong < 0 then
                                                from

                                            else if
                                                distanceAlong
                                                    > (from |> Point2d.distanceFrom to |> Pixels.toFloat)
                                            then
                                                to

                                            else
                                                Point2d.along axis (Pixels.float distanceAlong)

                                        length =
                                            bestApprox |> Point2d.distanceFrom target |> Pixels.toFloat
                                    in
                                    bestApprox
                                        |> Point2d.signedDistanceFrom targetAxis
                                        |> Pixels.toFloat
                                        |> (\signedDistance ->
                                                if length < 2 then
                                                    dist

                                                else if signedDistance < 0 then
                                                    dist
                                                        |> Tuple.mapFirst
                                                            (min
                                                                length
                                                            )

                                                else
                                                    dist
                                                        |> Tuple.mapSecond
                                                            (min
                                                                length
                                                            )
                                           )
                                )
                            |> Maybe.withDefault dist
                    )
                    ( maxDistance, maxDistance )



--( Nothing, Nothing )


step :
    Point2d Pixels ()
    -> Generator Data
    -> Generator Data
step to =
    Random.andThen
        (\{ from, iter, currentPath, previousPaths, searchList, drawings } ->
            let
                mid =
                    Point2d.midpoint from to

                maxLength =
                    Vector2d.from from mid
                        |> Vector2d.length
                        |> Pixels.toFloat
            in
            if maxLength < smallesLineSize then
                Random.constant
                    { from = to
                    , currentPath = []
                    , previousPaths = (from :: currentPath) :: previousPaths
                    , iter = iter
                    , searchList = searchList
                    , drawings = drawings
                    }

            else
                let
                    tupleMaybe =
                        searchList
                            |> findSmallestDistanceTo
                                { maxDistance = maxLength
                                , target = mid
                                , targetAxis =
                                    Axis2d.throughPoints mid to
                                        |> Maybe.withDefault Axis2d.x
                                }

                    scaleTo : Float -> Vector2d Pixels () -> Vector2d Pixels ()
                    scaleTo len v =
                        v
                            |> Vector2d.scaleBy
                                (len
                                    / (v
                                        |> Vector2d.length
                                        |> Pixels.toFloat
                                      )
                                )

                    genP1 : Generator (Point2d Pixels ())
                    genP1 =
                        Random.float (pi / 2 - pi / 2 * spread) (pi / 2 + pi / 2 * spread)
                            |> Random.map
                                (\r ->
                                    tupleMaybe
                                        |> Tuple.second
                                        |> (\radius ->
                                                mid
                                                    |> Point2d.translateBy
                                                        (Vector2d.from mid to
                                                            |> scaleTo radius
                                                            |> Vector2d.rotateBy (Angle.radians r)
                                                        )
                                           )
                                )

                    genP2 : Generator (Point2d Pixels ())
                    genP2 =
                        Random.float (3 * pi / 2 - pi / 2 * spread) (3 * pi / 2 + pi / 2 * spread)
                            |> Random.map
                                (\r ->
                                    tupleMaybe
                                        |> Tuple.first
                                        |> (\radius ->
                                                mid
                                                    |> Point2d.translateBy
                                                        (Vector2d.from mid to
                                                            |> scaleTo radius
                                                            |> Vector2d.rotateBy (Angle.radians r)
                                                        )
                                           )
                                )
                in
                Random.map2
                    (\p1 p2 ->
                        { from = to
                        , currentPath =
                            [ p2, from ]
                        , previousPaths = (to :: p1 :: from :: currentPath) :: previousPaths
                        , iter = iter
                        , searchList =
                            [ ( p1, to ), ( from, p1 ) ]
                                ++ [ ( p2, to ), ( from, p2 ) ]
                                ++ searchList
                        , drawings =
                            [ ( p1, to ), ( from, p1 ) ]
                                ++ [ ( p2, to ), ( from, p2 ) ]
                                ++ drawings
                        }
                    )
                    genP1
                    genP2
        )
