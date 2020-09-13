module LinesCards.Main exposing (main)

import Angle exposing (Angle)
import Circle2d exposing (Circle2d)
import Element
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Length exposing (Length)
import LinesCards.Card as Card exposing (Card(..), Color(..))
import LinesCards.Image as Image
import LinesCards.View as View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Rectangle2d exposing (Rectangle2d)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d, cross)


viewCard : Card -> List (Svg msg)
viewCard card =
    Image.view card
        ++ [ Rectangle2d.from Point2d.origin (Point2d.pixels View.width View.height)
                |> Svg.rectangle2d
                    [ Attributes.stroke "white"
                    , Attributes.strokeWidth <| String.fromFloat <| View.relative <| 2
                    , Attributes.fill <| "transparent"
                    ]
           ]


main : Html msg
main =
    let
        colors =
            [ Y, B, G, R ]
    in
    (colors
        |> List.concatMap
            (\c1 ->
                colors
                    |> List.map (\c2 -> Cross c1 c2)
            )
    )
        ++ ([ VEdges, HEdges ]
                |> List.concatMap
                    (\f ->
                        colors
                            |> List.indexedMap
                                (\i c1 ->
                                    colors
                                        |> List.drop i
                                        |> List.map (\c2 -> f c1 c2)
                                )
                            |> List.concat
                    )
           )
        |> List.map
            (\card ->
                viewCard card
                    |> Svg.svg
                        [ Attributes.width <| String.fromFloat <| View.zoom * View.width
                        , Attributes.height <| String.fromFloat <| View.zoom * View.height
                        , Attributes.viewBox <|
                            "0 0 "
                                ++ String.fromFloat View.width
                                ++ " "
                                ++ String.fromFloat View.height
                        ]
            )
        |> Html.div []



{--
--}
