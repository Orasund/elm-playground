module FactoryCity.View.Details exposing (view)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), MachineSort(..), MovableSort(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
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
                    |> Item.color
                    |> (\( r, g, b ) ->
                            Background.color <| Element.rgb255 r g b
                       )
               ]
        )
    <|
        Element.text (item |> Item.itemToString)


display : ContainerSort -> { name : String, desc : List (List (Element msg)) }
display containerSort =
    case containerSort of
        Crate item ->
            { name = (item |> Item.itemToString) ++ " Crate"
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
                        , Element.text <| " itself can be produced in different ways. Use the press to turn it back into "
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
            { name = "Railroad"
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
            { name = "Output"
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
                        [ Item.smeltable
                            |> List.concatMap
                                (\( from, to ) ->
                                    [ Element.text <| "Smelts "
                                    , Element.el
                                        (tag
                                            ++ [ from
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text (from |> Item.itemToString)
                                    , Element.text <| " into "
                                    , Element.el
                                        (tag
                                            ++ [ to
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text <|
                                            (to |> Item.itemToString)
                                    , Element.text <| ". "
                                    ]
                                )
                        , List.singleton <| Element.text <| "Needs either "
                        , Item.burnable
                            |> List.map
                                (\i ->
                                    Element.el
                                        (tag
                                            ++ [ i
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                    <|
                                        Element.text <|
                                            (i |> Item.itemToString)
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
                        [ Item.shreddable
                            |> List.concatMap
                                (\( from, to ) ->
                                    [ Element.text <| "Shredds "
                                    , Element.el
                                        (tag
                                            ++ [ from
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text (from |> Item.itemToString)
                                    , Element.text <| " into "
                                    , Element.el
                                        (tag
                                            ++ [ to
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text <|
                                            (to |> Item.itemToString)
                                    , Element.text <| ". "
                                    ]
                                )
                        , List.singleton <| Element.text <| " Needs either "
                        , Item.burnable
                            |> List.map
                                (\i ->
                                    Element.el
                                        (tag
                                            ++ [ i
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                    <|
                                        Element.text <|
                                            (i |> Item.itemToString)
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
                        [ Item.pressable
                            |> List.concatMap
                                (\( from, to ) ->
                                    [ Element.text <| "Compresses "
                                    , Element.el
                                        (tag
                                            ++ [ from
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text (from |> Item.itemToString)
                                    , Element.text <| " into "
                                    , Element.el
                                        (tag
                                            ++ [ to
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                      <|
                                        Element.text <|
                                            (to |> Item.itemToString)
                                    , Element.text <| ". "
                                    ]
                                )
                        , List.singleton <|
                            Element.text <|
                                " Needs either "
                        , Item.burnable
                            |> List.map
                                (\i ->
                                    Element.el
                                        (tag
                                            ++ [ i
                                                    |> Item.color
                                                    |> (\( r, g, b ) ->
                                                            Background.color <| Element.rgb255 r g b
                                                       )
                                               ]
                                        )
                                    <|
                                        Element.text <|
                                            (i |> Item.itemToString)
                                )
                            |> List.intersperse (Element.text " or ")
                        , List.singleton <|
                            Element.text <|
                                " for every usage."
                        ]
            }

        Removable _ ->
            { name = "Removable"
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
    , amount : Int
    }
    -> Element msg
view { selected, sellMsg, price, amount } =
    Element.column Grid.section <|
        List.singleton <|
            Element.column
                (Card.large
                    ++ [ Element.centerX ]
                )
            <|
                case selected of
                    Just card ->
                        let
                            { name, desc } =
                                display card
                        in
                        List.concat
                            [ [ Element.paragraph Heading.h3 <|
                                    [ Text.colored 24 <|
                                        CellType.containerSortToString <|
                                            card
                                    , Element.text <| " - " ++ name
                                    ]
                              , desc
                                    |> List.map (Element.paragraph [])
                                    |> Element.textColumn Grid.section
                              ]
                            , if amount >= 1 then
                                [ Element.row Grid.spaceEvenly
                                    [ Input.button Button.simple <|
                                        { label =
                                            Text.colored 16 <|
                                                "💲 sell for "
                                                    ++ (price
                                                            |> String.fromInt
                                                       )
                                        , onPress = Just <| sellMsg card
                                        }
                                    ]
                                ]

                              else
                                []
                            ]

                    Nothing ->
                        List.singleton <|
                            Element.paragraph
                                []
                            <|
                                List.singleton <|
                                    Element.text <|
                                        "Click on a card to view details"
