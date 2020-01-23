module FactoryCity.View.Crafting exposing (view)

import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), MachineSort(..), MovableSort(..))
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Grid.Direction as Direction exposing (Direction(..))


dirList : List Direction
dirList =
    [ Up, Left, Right, Down ]


view : { craftMsg : ContainerSort -> msg } -> Element msg
view { craftMsg } =
    [ ( CellType.tierOne
      , List.concat
            [ [ Machine Furnace { isWarm = False }
              , CellType.output
              ]
            , dirList
                |> List.concatMap
                    (\from ->
                        dirList
                            |> List.filterMap
                                (\to ->
                                    if from == to then
                                        Nothing

                                    else
                                        Just <| Movable Belt { from = from, to = to }
                                )
                    )
            ]
      )
    , ( CellType.tierTwo
      , List.concat
            [ [ Machine Shredder { isWarm = False }
              , Machine Press { isWarm = False }
              ]
            , dirList
                |> List.map
                    (\to -> Movable Merger { from = to |> Direction.flip, to = to })
            ]
      )
    ]
        |> List.map
            (\( fun, list ) ->
                Element.column (Card.simple ++ Grid.spaceEvenly)
                    [ Element.paragraph [ Element.width <| Element.fill ] <|
                        (fun
                            |> List.map
                                (\( item, n ) ->
                                    (n |> String.fromInt) ++ " " ++ (item |> CellType.itemToString)
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
                                            Text.view 16 <|
                                                CellType.containerSortToString <|
                                                    card
                                        }
                                )
                        )
                    ]
            )
        |> Element.column Grid.simple
        |> Element.el Grid.section
