module Generative.Point exposing (Point, smoothen)

import Point2d
import Vector2d exposing (Vector2d)


type alias Point =
    ( Float, Float )




smoothen : Int -> List Point -> List Point
smoothen n l =
    l
        |> List.foldl
            (\_ ( list, out ) ->
                case list of
                    ( x, y ) :: b :: tail ->
                        let
                            v : Vector2d
                            v =
                                Vector2d.from
                                    (Point2d.fromCoordinates ( x, y ))
                                    (Point2d.fromCoordinates b)

                            p1 : Point
                            p1 =
                                v
                                    |> Vector2d.scaleBy 0.333
                                    |> Vector2d.components
                                    |> Tuple.mapBoth ((+) x) ((+) y)

                            p2 : Point
                            p2 =
                                v
                                    |> Vector2d.scaleBy 0.666
                                    |> Vector2d.components
                                    |> Tuple.mapBoth ((+) x) ((+) y)
                        in
                        ( b :: tail, p2 :: p1 :: out )

                    _ ->
                        ( [], out )
            )
            ( l, [] )
        |> Tuple.second
        |> List.reverse
        |> (if n < 2 then
                identity

            else
                smoothen (n - 1)
           )
