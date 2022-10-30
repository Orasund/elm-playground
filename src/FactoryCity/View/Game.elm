module FactoryCity.View.Game exposing (arrangement, view)

import Bag
import Element exposing (Element)
import FactoryCity.Data as Data
import FactoryCity.Data.CellType exposing (ContainerSort(..))
import FactoryCity.Data.Game exposing (Game, Msg(..), Tab(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
import FactoryCity.View.Board as BoardView
import FactoryCity.View.Crafting as Crafting
import FactoryCity.View.Deck as DeckView
import FactoryCity.View.Details as Details
import FactoryCity.View.Settings as Settings
import FactoryCity.View.Shop as Shop
import Framework.Grid as Grid


arrangement : List (List Tab)
arrangement =
    [ [ ShopTab, MachineTab ]
    , [ GameTab ]
    , [ CraftingTab, DetailsTab ]
    ]


view :
    { scale : Float
    , selected : Maybe ContainerSort
    , positionSelectedMsg : ( Int, Int ) -> msg
    , selectedMsg : ContainerSort -> msg
    , craftMsg : ContainerSort -> msg
    , speed : Int
    , clickedChangeSpeedMsg : Int -> msg
    , msgMapper : Msg -> msg
    , clickedTierTabMsg : Int -> msg
    , displayedTier : Int
    , clickedViewInfoMsg : ContainerSort -> msg
    }
    -> Game
    -> Tab
    -> Element msg
view { clickedViewInfoMsg, displayedTier, clickedTierTabMsg, msgMapper, scale, clickedChangeSpeedMsg, speed, selected, positionSelectedMsg, selectedMsg, craftMsg } game tab =
    case tab of
        ShopTab ->
            Shop.view
                { shop = game.shop
                , buyMsg = Just <| ClickedBuy
                , sellMsg = Just <| ClickedSell
                , money = game.money
                , deck = game.deck
                }
                |> Element.map msgMapper

        MachineTab ->
            Settings.view
                { changedLoopLengthMsg = ChangedLoopLength >> msgMapper
                , loopLength = game.loopEvery
                , speed = speed
                , clickedChangeSpeedMsg = clickedChangeSpeedMsg
                , toggledBuyRegularlyMsg = ToggledBuyRegularly >> msgMapper
                , toggledSellRegularlyMsg =
                    ToggledSellRegularly
                        >> msgMapper
                , shoppingList = game.shoppingList
                , sellingList = game.sellingList
                }

        GameTab ->
            Element.column Grid.section <|
                [ game.board
                    |> BoardView.view
                        { scale = scale
                        , maybePositionMsg = Just positionSelectedMsg
                        }
                , game.deck |> DeckView.view scale (Just selectedMsg) selected
                ]

        CraftingTab ->
            Crafting.view
                { displayedTier = displayedTier
                , craftMsg = craftMsg
                , clickedTierTabMsg = clickedTierTabMsg
                , clickedViewInfoMsg = clickedViewInfoMsg
                }

        DetailsTab ->
            let
                price : Int
                price =
                    (case selected of
                        Just (Crate item) ->
                            item

                        _ ->
                            Scrap
                    )
                        |> (\item -> max 1 <| Data.maxPrice // ((game.shop |> Bag.count (item |> Item.itemToString)) + 1))
            in
            Details.view
                { amount =
                    selected
                        |> Maybe.map
                            (\card ->
                                game.deck
                                    |> Bag.count (card |> FactoryCity.Data.CellType.containerSortToString)
                            )
                        |> Maybe.withDefault 0
                , selected = selected
                , sellMsg = ClickedSell
                , price = price
                }
                |> Element.map msgMapper
