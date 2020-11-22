module LinesCards.Main exposing (main)

import Geometry.Svg as Svg
import Html exposing (Html)
import LinesCards.Card exposing (Card(..), Color(..))
import LinesCards.Image as Image
import LinesCards.View as View
import Point2d
import Rectangle2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes


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
