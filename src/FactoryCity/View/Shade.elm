module FactoryCity.View.Shade exposing (viewNormal, viewWon)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Card as Card
import Framework.Grid as Grid


viewNormal : List (Element msg) -> Element msg
viewNormal content =
    view
        [ Background.color <| Element.rgba255 0 0 0 0.7
        , Font.color <| Element.rgb255 255 255 255
        ]
        content


viewWon : List (Element msg) -> Element msg
viewWon content =
    view
        [ Background.color <| Element.rgba255 204 166 0 0.7
        , Font.color <| Element.rgb255 0 0 0
        ]
        content


view : List (Attribute msg) -> List (Element msg) -> Element msg
view attributes content =
    Element.el
        (attributes
            |> List.append
                (Card.simple
                    ++ [ Element.width <| Element.fill
                       , Element.height <| Element.fill
                       , Border.width <| 0
                       , Border.rounded <| 0
                       , Element.centerX
                       , Element.centerY
                       , Font.family
                            [ Font.sansSerif ]
                       ]
                )
        )
    <|
        Element.column (Grid.spaceEvenly ++ [ Element.height <| Element.shrink ]) <|
            content
