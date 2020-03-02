module Katakomben.View.Deck exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Framework.Color as Color
import Framework.Grid as Grid
import Katakomben.Data.Card exposing (Card)
import Katakomben.Data.CardDetails as CardDetails
import Katakomben.Data.Deck as Deck exposing (Deck)


view : Deck Card -> Element msg
view deck =
    Element.row Grid.compact
        [ Element.el [ Element.width <| Element.fill ] <| Element.none
        , deck
            |> Deck.toList
            |> List.map
                (CardDetails.getDetails
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
            |> Element.row (Grid.simple ++ [ Element.clipX ])
        ]
