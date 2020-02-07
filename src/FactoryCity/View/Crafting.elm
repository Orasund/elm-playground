module FactoryCity.View.Crafting exposing (view)

import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), MachineSort(..), MovableSort(..))
import FactoryCity.Data.Item as Item
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Grid.Direction exposing (Direction(..))


view : { craftMsg : ContainerSort -> msg } -> Element msg
view { craftMsg } =
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
        |> List.map
            (\( fun, list ) ->
                Element.column (Card.simple ++ Grid.spaceEvenly)
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
                                    Input.button Button.simple
                                        { onPress = Just <| craftMsg <| card
                                        , label =
                                            Text.colored 16 <|
                                                CellType.containerSortToString <|
                                                    card
                                        }
                                )
                        )
                    ]
            )
        |> Element.column Grid.simple
        |> Element.el Grid.section
