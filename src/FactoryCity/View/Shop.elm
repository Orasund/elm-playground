module FactoryCity.View.Shop exposing (view)

import Bag exposing (Bag)
import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data as Data
import FactoryCity.Data.CellType as CellType exposing (ContainerSort)
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading


view :
    { shop : Bag String
    , buyMsg : String -> msg
    , money : Int
    }
    -> Element msg
view { shop, buyMsg, money } =
    Element.column (Grid.simple ++ [ Element.width <| Element.shrink ]) <|
        [ Element.row Grid.spaceEvenly <|
            [ Element.el Heading.h2 <| Element.text "Shop"
            , Element.text <| "Money:" ++ (money |> String.fromInt)
            ]
        , Element.column Grid.section <|
            (shop
                |> Bag.toList
                |> List.map
                    (\( card, n ) ->
                        let
                            cost : Int
                            cost =
                                max 1 <| Data.maxPrice // n
                        in
                        Element.row (Card.small ++ Grid.spaceEvenly) <|
                            [ Text.view 16 <| card
                            , if cost <= money then
                                Input.button Button.simple <|
                                    { onPress = Just <| buyMsg <| card
                                    , label =
                                        Element.text <|
                                            "buy for "
                                                ++ (cost |> String.fromInt)
                                    }

                              else
                                Element.none
                            ]
                    )
            )
        ]
