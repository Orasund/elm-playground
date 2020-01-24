module FactoryCity.View.Details exposing (view)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), Item(..), MachineSort(..), MovableSort(..))
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Heading as Heading


tag : List (Attribute msg)
tag =
    [ Border.rounded 4
    , Element.paddingXY 7 2 --4
    ]


displayItem : Item -> Element msg
displayItem item =
    Element.el
        (tag
            ++ [ item
                    |> CellType.color
                    |> (\( r, g, b ) ->
                            Background.color <| Element.rgb255 r g b
                       )
               ]
        )
    <|
        Element.text (item |> CellType.itemToString)


display : ContainerSort -> { name : String, desc : List (List (Element msg)) }
display containerSort =
    case containerSort of
        Crate item ->
            { name = (item |> CellType.itemToString) ++ " Crate"
            , desc =
                [ [ Element.text <| "Outputs one "
                  , displayItem <| item
                  , Element.text <| " at the start of each loop. Nothing will happen if you do not have any "
                  , displayItem <| item
                  , Element.text <| "."
                  ]
                , case item of
                    Wood ->
                        [ Element.text <| " Use "
                        , displayItem <| Wood
                        , Element.text <| " as your primary fuel resource. Alternatively "
                        , displayItem <| Wood
                        , Element.text <| " can be turned into "
                        , displayItem <| Chips
                        , Element.text <| " using the shredder."
                        ]

                    Stone ->
                        [ Element.text <| " Smelt "
                        , displayItem <| Stone
                        , Element.text <| " into "
                        , displayItem <| Iron
                        , Element.text <| " using the furnace."
                        ]

                    Scrap ->
                        [ Element.text <| " "
                        , displayItem <| Scrap
                        , Element.text <| " itself have be produced in different ways. Use the press to turn it back into "
                        , displayItem <| Stone
                        , Element.text <| "."
                        ]

                    Chips ->
                        [ Element.text <| " If done correctly, "
                        , displayItem <| Chips
                        , Element.text <| " is the best fuel available."
                        ]

                    Iron ->
                        [ Element.text <| " Craft "
                        , displayItem <| Iron
                        , Element.text <| " into items."
                        ]

                    Chipboard ->
                        [ Element.text <| " Craft "
                        , displayItem <| Chipboard
                        , Element.text <| " into items."
                        ]
                ]
            }

        Movable Belt _ ->
            { name = "Conveyor Belt"
            , desc =
                List.singleton <|
                    [ Element.text <| "Transports an item. If an output has multiple conveyor belts attached, it will turn into "
                    , displayItem <| Scrap
                    , Element.text <| ". Similarly if two belts try to push an item onto the same tile, one of the items will be permantly deleted."
                    ]
            }

        Movable Merger _ ->
            { name = "Merger"
            , desc =
                List.singleton <|
                    List.singleton <|
                        Element.text <|
                            "Merges items from three directions into one. This merger will avoid merging conflicts, thus its safer then a usual conveyor belt. But its also three times slower."
            }

        Output ->
            { name = "Lorry"
            , desc =
                List.singleton <|
                    List.singleton <|
                        Element.text <|
                            "Returns items back into your hand. Only move one item at the time into the lorry or else an item might be deleted."
            }

        Machine Furnace _ ->
            { name = "Furnace"
            , desc =
                List.singleton <|
                    List.concat
                        [ CellType.smeltable
                            |> List.concatMap
                                (\( from, to ) ->
                                    [ Element.text <| "Smelts "
                                    , Element.el
                                        (tag
                                            ++ [ from
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text (from |> CellType.itemToString)
                                    , Element.text <| " into "
                                    , Element.el
                                        (tag
                                            ++ [ to
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text <|
                                            (to |> CellType.itemToString)
                                    , Element.text <| ". "
                                    ]
                                )
                        , List.singleton <| Element.text <| "Needs either "
                        , CellType.burnable
                            |> List.map
                                (\i ->
                                    Element.el
                                        (tag
                                            ++ [ i
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                    <|
                                        Element.text <|
                                            (i |> CellType.itemToString)
                                )
                            |> List.intersperse (Element.text " or ")
                        , List.singleton <| Element.text <| " at the beginning of each cycle."
                        ]
            }

        Machine Shredder _ ->
            { name = "Shredder"
            , desc =
                List.singleton <|
                    List.concat
                        [ CellType.shreddable
                            |> List.concatMap
                                (\( from, to ) ->
                                    [ Element.text <| "Shredds "
                                    , Element.el
                                        (tag
                                            ++ [ from
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text (from |> CellType.itemToString)
                                    , Element.text <| " into "
                                    , Element.el
                                        (tag
                                            ++ [ to
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text <|
                                            (to |> CellType.itemToString)
                                    , Element.text <| ". "
                                    ]
                                )
                        , List.singleton <| Element.text <| " Needs either "
                        , CellType.burnable
                            |> List.map
                                (\i ->
                                    Element.el
                                        (tag
                                            ++ [ i
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                    <|
                                        Element.text <|
                                            (i |> CellType.itemToString)
                                )
                            |> List.intersperse (Element.text " or ")
                        , List.singleton <| Element.text <| " at the beginning of each cycle. It procudes up to 3 items and will store the additional items in adjacent crates!"
                        ]
            }

        Machine Press _ ->
            { name = "Press"
            , desc =
                List.singleton <|
                    List.concat
                        [ CellType.pressable
                            |> List.concatMap
                                (\( from, to ) ->
                                    [ Element.text <| "Compresses "
                                    , Element.el
                                        (tag
                                            ++ [ from
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text (from |> CellType.itemToString)
                                    , Element.text <| " into "
                                    , Element.el
                                        (tag
                                            ++ [ to
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text <|
                                            (to |> CellType.itemToString)
                                    , Element.text <| ". "
                                    ]
                                )
                        , List.singleton <|
                            Element.text <|
                                " Needs either "
                        , CellType.burnable
                            |> List.map
                                (\i ->
                                    Element.el
                                        (tag
                                            ++ [ i
                                                    |> CellType.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                    <|
                                        Element.text <|
                                            (i |> CellType.itemToString)
                                )
                            |> List.intersperse (Element.text " or ")
                        , List.singleton <|
                            Element.text <|
                                " for every usage."
                        ]
            }

        Bug ->
            { name = "Bug"
            , desc =
                List.singleton <|
                    List.singleton <|
                        Element.text <|
                            "This is a Bug"
            }


view :
    { selected : Maybe ContainerSort
    , sellMsg : ContainerSort -> msg
    , price : Int
    }
    -> Element msg
view { selected, sellMsg, price } =
    Element.column Grid.section <|
        List.singleton <|
            Element.column Card.large <|
                case selected of
                    Just card ->
                        let
                            { name, desc } =
                                display card
                        in
                        [ Element.paragraph Heading.h3 <|
                            [ Text.view 24 <|
                                CellType.containerSortToString <|
                                    card
                            , Element.text <| " - " ++ name
                            ]
                        , desc
                            |> List.map (Element.paragraph [])
                            |> Element.textColumn Grid.section
                        , Input.button Button.simple <|
                            { label =
                                Text.view 16 <|
                                    "💲 sell for "
                                        ++ (price
                                                |> String.fromInt
                                           )
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
