module HeroForge.View.Deck exposing (view)

import Element exposing (Element)
import Element.Border as Border
import Framework.Color as Color
import Framework.Grid as Grid
import HeroForge.Data.Card as Card exposing (Card)
import HeroForge.Data.Deck as Deck exposing (Deck)


view : Deck Card -> Element msg
view deck =
    deck
        |> Deck.toList
        |> List.map
            (Card.toString
                >> .color
                >> (\color ->
                        Element.el
                            ([ Element.width <| Element.px <| 10
                             , Element.height <| Element.px <| 10
                             , Border.color <| Color.lightGrey
                             , Border.width 1
                             , Border.rounded 5
                             ]
                                ++ (color |> List.map (Element.mapAttribute never))
                            )
                        <|
                            Element.none
                   )
            )
        |> Element.row (Grid.simple ++ [ Element.clipX, Element.centerY ])
