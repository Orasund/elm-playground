module FactoryCity.View.Deck exposing (view)

import Card
import Element exposing (Attribute, Element)
import Element.Background as Background
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..))
import FactoryCity.Data.Deck as Deck exposing (Deck)
import FactoryCity.Data.Item as Item
import FactoryCity.View.Text as Text
import Framework.Grid as Grid


viewContent : ContainerSort -> Int -> Element msg
viewContent containerSort n =
    Element.column
        Grid.simple
    <|
        [ Element.paragraph [] <|
            List.singleton <|
                Text.view 16 <|
                    CellType.containerSortToString containerSort
        , Element.text <| String.fromInt <| n
        ]


viewAttributes : Float -> List (Attribute msg)
viewAttributes scale =
    [ Element.centerX
    , Element.spaceEvenly
    , Element.height <| Element.px <| floor <| 200 * scale
    , Element.width <| Element.fill
    ]


view :
    Float
    -> Maybe (ContainerSort -> msg)
    -> Maybe ContainerSort
    -> Deck
    -> Element msg
view scale maybeSelectedMsg maybeSelected deck =
    Element.row (viewAttributes scale) <|
        [ Card.hand
            [ Element.centerX
            , Element.height <| Element.px <| floor <| 200 * scale
            ]
            { width = 400 * scale
            , dimensions = ( 120 * scale, 176 * scale )
            , cards =
                deck
                    |> Deck.toList
                    |> List.map
                        (\( containerSort, n ) ->
                            Card.card
                                { attributes =
                                    case containerSort of
                                        Crate item ->
                                            item
                                                |> Item.color
                                                |> (\( r, g, b ) ->
                                                        [ Background.color <| Element.rgb255 r g b ]
                                                   )

                                        _ ->
                                            []
                                , content = viewContent containerSort n
                                , onPress = maybeSelectedMsg |> Maybe.map (\fun -> fun containerSort)
                                , selected = maybeSelected == Just containerSort
                                }
                        )
            }
        ]
