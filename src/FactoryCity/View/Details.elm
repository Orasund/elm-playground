module FactoryCity.View.Details exposing (view)

import Element exposing (Element)
import Element.Input as Input
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), Item(..), MachineSort(..), MovableSort(..))
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading


display : ContainerSort -> { name : String, desc : String }
display containerSort =
    case containerSort of
        Crate Wood ->
            { name = "Wood Crate"
            , desc = "Outputs one wood at the start of each loop. Nothing will happen if you do not have any wood. Use wood as your primary fuel resource. Alternatively wood can be turned into chips using the shredder."
            }

        Crate Stone ->
            { name = "Stone Crate"
            , desc = "Outputs one stone at the start of each loop. Nothing will happen if you do not have any stone. Smelt stone into iron using the furnace."
            }

        Crate Scrap ->
            { name = "Scrap Crate"
            , desc = "Outputs scrap at the start of each loop. Nothing will happen if you do not have any scrap. Scrap itself have be produced in different ways. Use the press to turn it back into stone"
            }

        Crate Chips ->
            { name = "Chips Crate"
            , desc = "Outputs chips at the start of each loop. Nothing will happen if you do not have any chips. If done correctly, chips is the best fuel available."
            }

        Crate Iron ->
            { name = "Iron Crate"
            , desc = "Outputs one iron at the start of each loop. Nothing will happen if you do not have any iron. Craft iron into items."
            }

        Movable Belt _ ->
            { name = "Conveyor Belt"
            , desc = "Transports an item. If an output has multiple conveyor belts attached, it will turn into scrap. Similarly if two belts try to push an item onto the same tile, one of the items will be permantly deleted."
            }

        Movable Merger _ ->
            { name = "Merger"
            , desc = "Merges items from three directions into one. This merger will avoid merging conflicts, thus its safer then a usual conveyor belt. But its also three times slower."
            }

        Output ->
            { name = "Lorry"
            , desc = "Returns items back into your hand. Only move one item at the time into the lorry or else an item might be deleted."
            }

        Machine Furnace _ ->
            { name = "Furnace"
            , desc = "Smelts items. Always need a fuel at the beginning of each cycle (like wood or chips). Once its activated, it can turn stone into iron."
            }

        Machine Shredder _ ->
            { name = "Shredder"
            , desc = "Chops items into smaller items. Need fuel at the beginning of each cycle to function. One running it will turn wood into chips. It will also fill nearby crates!"
            }

        Machine Press _ ->
            { name = "Press"
            , desc = "Compresses items. Needs fuel for every usabe, but in return it will compress scrap back into stone."
            }

        Bug ->
            { name = "Bug"
            , desc = "This is a Bug"
            }


view : { selected : Maybe ContainerSort, sellMsg : ContainerSort -> msg, price : ContainerSort -> Int } -> Element msg
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
                        , Element.paragraph Grid.section <|
                            List.singleton <|
                                Element.text <|
                                    desc
                        , Input.button Button.simple <|
                            { label =
                                Element.text <|
                                    "sell for "
                                        ++ ((case card of
                                                Crate _ ->
                                                    card

                                                _ ->
                                                    Crate Scrap
                                            )
                                                |> price
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
