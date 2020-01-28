module FactoryCity.View.Shop exposing (view)

import Bag exposing (Bag)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FactoryCity.Data as Data
import FactoryCity.Data.CellType as CellType exposing (ContainerSort)
import FactoryCity.Data.Item as Item exposing (Item)
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Color as Color
import Framework.Grid as Grid


view :
    { shop : Bag String
    , buyMsg : Maybe (Item -> Int -> msg)
    , sellMsg : Maybe (ContainerSort -> msg)
    , money : Int
    , deck : Bag String
    }
    -> Element msg
view { shop, buyMsg, sellMsg, money, deck } =
    Element.el Grid.section <|
        Element.column Grid.simple <|
            [ Element.text <| "Money:" ++ (money |> String.fromInt)
            , Element.column Grid.simple <|
                (shop
                    |> Bag.toList
                    |> List.filterMap
                        (\( c, n ) ->
                            c
                                |> Item.stringToItem
                                |> Maybe.map (\a -> ( a, n ))
                        )
                    |> List.map
                        (\( item, n ) ->
                            let
                                cost : Int
                                cost =
                                    max 1 <| Data.maxPrice // n
                            in
                            Element.row
                                Grid.simple
                            <|
                                [ Element.el [ Element.width <| Element.fill ] <|
                                    Element.el
                                        (item
                                            |> Item.color
                                            |> (\( r, g, b ) ->
                                                    [ Background.color <|
                                                        Element.rgb255 r g b
                                                    , Border.rounded 20
                                                    , Element.paddingXY 16 12
                                                    ]
                                               )
                                        )
                                    <|
                                        Text.view 16 <|
                                            Item.itemToString <|
                                                item
                                , Maybe.map2
                                    (\bMsg sMsg ->
                                        Element.row (Grid.compact ++ [ Element.width <| Element.fill ])
                                            [ Input.button
                                                (Button.groupLeft
                                                    ++ (case deck |> Bag.count (CellType.crate item |> CellType.containerSortToString) of
                                                            0 ->
                                                                Color.disabled

                                                            _ ->
                                                                []
                                                       )
                                                    ++ [ Element.width <| Element.fill ]
                                                )
                                              <|
                                                { onPress = Just <| sMsg (CellType.crate <| item)
                                                , label =
                                                    Element.row Grid.spaceEvenly <|
                                                        [ Text.view 16 <| "💲"
                                                        , Element.text <| String.fromInt (max 1 <| Data.maxPrice // (n + 1))
                                                        ]
                                                }
                                            , Input.button
                                                (Button.groupRight
                                                    ++ (if cost <= money then
                                                            Color.primary

                                                        else
                                                            Color.disabled
                                                       )
                                                    ++ [ Element.width <| Element.fill ]
                                                )
                                              <|
                                                { onPress = Just <| bMsg item 1
                                                , label =
                                                    Element.row Grid.spaceEvenly <|
                                                        [ Text.view 16 <| "\u{1F6D2}"
                                                        , Element.text <| (cost |> String.fromInt)
                                                        ]
                                                }
                                            ]
                                    )
                                    buyMsg
                                    sellMsg
                                    |> Maybe.withDefault
                                        ("Selling for "
                                            ++ (Data.maxPrice
                                                    // (n + 1)
                                                    |> max 1
                                                    |> String.fromInt
                                               )
                                            |> Element.text
                                        )
                                ]
                        )
                )
            ]
