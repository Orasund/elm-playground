module FactoryCity.View.Shop exposing (view)

import Bag exposing (Bag)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FactoryCity.Data as Data
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..))
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading


view :
    { shop : Bag String
    , buyMsg : Maybe (String -> msg)
    , money : Int
    }
    -> Element msg
view { shop, buyMsg, money } =
    Element.el Grid.section <|
        Element.column Grid.simple <|
            [ Element.text <| "Money:" ++ (money |> String.fromInt)
            , Element.column Grid.simple <|
                (shop
                    |> Bag.toList
                    |> List.map
                        (\( card, n ) ->
                            let
                                cost : Int
                                cost =
                                    max 1 <| Data.maxPrice // n
                            in
                            Element.row
                                Grid.spaceEvenly
                            <|
                                [ Element.el
                                    (case card |> CellType.stringToContainerSort of
                                        Just (Crate item) ->
                                            item
                                                |> CellType.color
                                                |> (\( r, g, b ) ->
                                                        [ Background.color <| Element.rgb255 r g b
                                                        , Border.rounded 20
                                                        , Element.paddingXY 16 12
                                                        ]
                                                   )

                                        _ ->
                                            []
                                    )
                                  <|
                                    Text.view 16 <|
                                        card
                                , buyMsg
                                    |> Maybe.map
                                        (\msg ->
                                            Input.button
                                                (Button.simple
                                                    ++ (if cost <= money then
                                                            []

                                                        else
                                                            Color.disabled
                                                       )
                                                )
                                            <|
                                                { onPress = Just <| msg <| card
                                                , label =
                                                    Element.text <|
                                                        "Buy for "
                                                            ++ (cost |> String.fromInt)
                                                }
                                        )
                                    |> Maybe.withDefault
                                        ("Selling for "
                                            ++ (Data.maxPrice // (n + 1) |> max 1 |> String.fromInt)
                                            |> Element.text
                                        )
                                ]
                        )
                )
            ]
