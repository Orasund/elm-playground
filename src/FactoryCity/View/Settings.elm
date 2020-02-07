module FactoryCity.View.Settings exposing (view)

import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data.Item as Item exposing (Item)
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group
import Framework.Input as Input
import Set exposing (Set)


view :
    { changedLoopLengthMsg : Int -> msg
    , loopLength : Int
    , speed : Int
    , clickedChangeSpeedMsg : Int -> msg
    , toggledBuyRegularlyMsg : Item -> msg
    , toggledSellRegularlyMsg : Item -> msg
    , shoppingList : Set String
    , sellingList : Set String
    }
    -> Element msg
view { shoppingList, sellingList, changedLoopLengthMsg, loopLength, speed, clickedChangeSpeedMsg, toggledSellRegularlyMsg, toggledBuyRegularlyMsg } =
    Element.column Grid.section <|
        [ Element.row Grid.spaceEvenly <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text <|
                        "Cycle Length"
            , [ 5, 10, 15, 20 ]
                |> List.indexedMap
                    (\i n ->
                        Input.button
                            ((if i == 0 then
                                Button.groupLeft

                              else if i == 3 then
                                Button.groupRight

                              else
                                Button.groupCenter
                             )
                                ++ (if loopLength == n then
                                        Color.primary

                                    else
                                        []
                                   )
                            )
                            { onPress = Just <| changedLoopLengthMsg <| n
                            , label = Element.text <| String.fromInt <| n
                            }
                    )
                |> Element.row
                    (Grid.compact
                        ++ [ Element.alignRight
                           , Element.width Element.shrink
                           ]
                    )
            ]
        , Element.row Grid.spaceEvenly <|
            [ Element.paragraph [] <|
                List.singleton <|
                    Element.text <|
                        "Speed"
            , [ ( 0, "Pause" ), ( 1, "1" ), ( 2, "2" ) ]
                |> List.indexedMap
                    (\i ( n, label ) ->
                        Input.button
                            (List.concat
                                [ Button.simple
                                , if speed == n then
                                    Color.primary

                                  else
                                    []
                                , if i == 0 then
                                    Group.left

                                  else if i == 2 then
                                    Group.right

                                  else
                                    Group.center
                                ]
                            )
                            { onPress = Just <| clickedChangeSpeedMsg n
                            , label =
                                Text.colored 16 <| label
                            }
                    )
                |> Element.row
                    (Grid.compact
                        ++ [ Element.alignRight
                           , Element.width Element.shrink
                           ]
                    )
            ]
        , Element.paragraph [] <|
            List.singleton <|
                Element.text <|
                    "Buy each cycle"
        , Item.itemList
            |> List.map
                (\item ->
                    Input.button
                        (List.concat
                            [ Button.simple
                            , if
                                shoppingList
                                    |> Set.member
                                        (item |> Item.itemToString)
                              then
                                Color.primary

                              else
                                []
                            ]
                        )
                        { onPress =
                            Just <|
                                toggledBuyRegularlyMsg <|
                                    item
                        , label =
                            Text.colored 16 <|
                                Item.itemToString <|
                                    item
                        }
                )
            |> Element.wrappedRow Grid.simple
        , Element.paragraph [] <|
            List.singleton <|
                Element.text <|
                    "Sell all"
        , Item.itemList
            |> List.map
                (\item ->
                    Input.button
                        (List.concat
                            [ Button.simple
                            , if
                                sellingList
                                    |> Set.member
                                        (item |> Item.itemToString)
                              then
                                Color.primary

                              else
                                []
                            ]
                        )
                        { onPress =
                            Just <|
                                toggledSellRegularlyMsg <|
                                    item
                        , label =
                            Text.colored 16 <|
                                Item.itemToString <|
                                    item
                        }
                )
            |> Element.wrappedRow Grid.simple
        ]
