module FactoryCity.View.Details exposing (view)

import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort)
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading


view : { selected : Maybe ContainerSort, sellMsg : ContainerSort -> msg, price : Int } -> Element msg
view { selected, sellMsg, price } =
    Element.column Grid.section <|
        List.singleton <|
            Element.column Card.small <|
                case selected of
                    Just card ->
                        [ Element.el Heading.h3 <|
                            Text.view 24 <|
                                CellType.containerSortToString <|
                                    card
                        , Input.button Button.simple <|
                            { label = Element.text <| "sell for " ++ (price |> String.fromInt)
                            , onPress = Just <| sellMsg <| card
                            }
                        ]

                    Nothing ->
                        List.singleton <|
                            Element.paragraph
                                []
                            <|
                                List.singleton <|
                                    Element.text <|
                                        "Click on a card to view details"
