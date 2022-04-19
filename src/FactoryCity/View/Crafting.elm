module FactoryCity.View.Crafting exposing (view)

import Array
import Direction exposing (Direction(..))
import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), MachineSort(..), MovableSort(..))
import FactoryCity.Data.Item as Item
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Group as Group


view : { clickedViewInfoMsg : ContainerSort -> msg, displayedTier : Int, craftMsg : ContainerSort -> msg, clickedTierTabMsg : Int -> msg } -> Element msg
view { clickedViewInfoMsg, displayedTier, craftMsg, clickedTierTabMsg } =
    let
        tiers =
            [ ( CellType.tierOne
              , CellType.tierOneList
              )
            , ( CellType.tierTwo
              , CellType.tierTwoList
              )
            , ( CellType.tierThree
              , CellType.tierThreeList
              )
            ]
    in
    Element.column Grid.compact
        [ List.range 0 ((tiers |> List.length) - 1)
            |> List.map
                (\i ->
                    Input.button
                        (Button.simple
                            ++ Group.top
                            ++ (if displayedTier == i then
                                    Color.primary

                                else
                                    []
                               )
                        )
                        { onPress = Just <| clickedTierTabMsg i
                        , label =
                            Element.text <|
                                "Tier "
                                    ++ String.fromInt (i + 1)
                        }
                )
            |> Element.row Grid.spaceEvenly
        , tiers
            |> Array.fromList
            |> Array.get displayedTier
            |> Maybe.map
                (\( fun, list ) ->
                    Element.column (Card.simple ++ Group.bottom ++ Grid.spaceEvenly)
                        [ Element.paragraph [ Element.width <| Element.fill ] <|
                            (fun
                                |> List.map
                                    (\( item, n ) ->
                                        (n |> String.fromInt) ++ " " ++ (item |> Item.itemToString)
                                    )
                                |> String.join " and "
                                |> Element.text
                                |> List.singleton
                            )
                        , Element.wrappedRow (Grid.section ++ [ Element.width <| Element.fill ])
                            (list
                                |> List.map
                                    (\card ->
                                        Element.row (Grid.compact ++ [ Element.width <| Element.shrink ])
                                            [ Input.button (Button.simple ++ Color.info ++ Group.left)
                                                { onPress = Just <| clickedViewInfoMsg <| card
                                                , label =
                                                    Element.text <| "i"
                                                }
                                            , Input.button (Button.simple ++ Group.right)
                                                { onPress = Just <| craftMsg <| card
                                                , label =
                                                    Text.colored 16 <|
                                                        CellType.containerSortToString <|
                                                            card
                                                }
                                            ]
                                    )
                            )
                        ]
                )
            |> Maybe.withDefault (Element.el Card.simple <| Element.none)
        ]
        |> Element.el Grid.section
