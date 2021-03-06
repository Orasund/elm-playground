module HeroForge.View.Deck exposing (view)

import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Grid as Grid
import HeroForge.Data.Card as Card exposing (Card)
import HeroForge.Data.Deck as Deck exposing (Deck)


view : Deck Card -> Element msg
view deck =
    deck
        |> Deck.toList
        |> List.map
            (Card.toString
                >> (\{ symbol, color } ->
                        symbol
                            |> Element.text
                            |> Element.el [ Element.centerX, Element.centerY ]
                            |> Element.el
                                ([ Element.width <| Element.px <| 20
                                 , Element.height <| Element.px <| 20
                                 , Font.size 10
                                 , Border.rounded 10
                                 ]
                                    ++ (color
                                            |> Color.toRgba
                                            |> Element.fromRgb
                                            |> Background.color
                                            |> List.singleton
                                       )
                                )
                   )
            )
        |> Element.row (Grid.simple ++ [ Element.spacing 5, Element.clipX, Element.centerY ])
