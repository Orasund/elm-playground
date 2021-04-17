module HermeticMind.OracleCards exposing (main)

import Direction2d
import Geometry.Svg as Svg
import HermeticMind.Data.Card as Card exposing (Card(..))
import HermeticMind.View.BinarySigil as Sigil
import HermeticMind.View.Card as Card
import HermeticMind.View.Color as Color
import Html exposing (Html)
import Point2d
import Polyline2d
import Rectangle2d
import Svg exposing (Svg)
import Svg.Attributes as Attributes




main : Html msg
main =
    Back :: Card.asList
        |> List.map
            (\card ->
                Card.view card
                    |> Svg.svg
                        [ Attributes.width <| (String.fromFloat <| Card.zoom * Card.width) ++ "px"
                        , Attributes.height <| (String.fromFloat <| Card.zoom * Card.height) ++ "px"
                        , Attributes.version <| "1.1"
                        , Attributes.viewBox <|
                            "0 0 "
                                ++ String.fromFloat Card.width
                                ++ " "
                                ++ String.fromFloat Card.height
                        ]
            )
        |> Html.div []



{--
--}
