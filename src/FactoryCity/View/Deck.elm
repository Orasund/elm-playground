module FactoryCity.View.Deck exposing (view)

import Card
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Font as Font
import FactoryCity.Data.CellType as CellType exposing (CellType, ContainerSort(..))
import FactoryCity.Data.Deck as Deck exposing (Deck)
import FactoryCity.View.Text as Text
import Framework.Grid as Grid


viewInactiveCard : Float -> Element msg -> Element msg
viewInactiveCard scale content =
    Element.el
        [ Element.width <| Element.px <| floor <| 120 * scale
        , Element.height <| Element.px <| floor <| 176 * scale
        , Element.alignTop
        , Element.padding <| floor <| 5 * scale
        ]
    <|
        content


viewCardList : Float -> { sort : Bool } -> List CellType -> Element msg
viewCardList scale { sort } =
    List.map CellType.toString
        >> (if sort then
                List.sort

            else
                identity
           )
        >> List.map (\( name, item ) -> Element.text name)
        >> Element.wrappedRow
            [ Font.size <| floor <| 25 * scale
            , Element.spacing <| floor <| 5 * scale
            , Element.centerX
            ]


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
    -> { sort : Bool }
    -> Maybe (ContainerSort -> msg)
    -> Maybe ContainerSort
    -> Deck
    -> Element msg
view scale sort maybeSelectedMsg maybeSelected deck =
    Element.row (viewAttributes scale) <|
        [ Card.hand
            [ Element.centerX
            , Element.height <| Element.px <| floor <| 200 * scale
            ]
            { width = 250 * scale
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
                                                |> CellType.color
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
