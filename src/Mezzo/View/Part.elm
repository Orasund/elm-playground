module Mezzo.View.Part exposing (view)

import Element exposing (Element)
import Element.Border as Border
import Mezzo.Data.Card as Card exposing (CardPart, CardSort(..))
import Mezzo.View.Card as Card
import Mezzo.View.PartBubble as PartBubble
import Mezzo.View.Suit as Suit
import Widget.Material.Color as MaterialColor


view : List CardPart -> Element msg
view list =
    case list |> List.reverse of
        [ first, second ] ->
            [ Card.viewSmall (( first, second ) |> Card.fromParts)
            , PartBubble.viewJoined ( first, second )
            ]
                |> Element.column
                    [ Element.spacing 10
                    ]

        [ first ] ->
            first
                |> PartBubble.view
                    [ Element.alignBottom
                    ]

        _ ->
            Element.none
