module FactoryCity.View.Game exposing (view)

import Bag as Bag exposing (Bag)
import Element exposing (Element)
import FactoryCity.Data as Data
import FactoryCity.Data.CellType exposing (ContainerSort(..))
import FactoryCity.Data.Game exposing (EndCondition(..), Game)
import FactoryCity.Data.Item as Item exposing (Item(..))
import FactoryCity.View.Board as BoardView
import FactoryCity.View.Crafting as Crafting
import FactoryCity.View.Deck as DeckView
import FactoryCity.View.Details as Details
import FactoryCity.View.Settings as Settings
import FactoryCity.View.Shop as Shop
import Framework.Grid as Grid
import Framework.Heading as Heading
import Grid.Position exposing (Position)


view :
    { counter : Int
    , money : Int
    , shop : Bag String
    , scale : Float
    , selected : Maybe ContainerSort
    , loopLength : Int
    , positionSelectedMsg : Position -> msg
    , selectedMsg : ContainerSort -> msg
    , buyMsg : Item -> Int -> msg
    , sellMsg : ContainerSort -> Int -> msg
    , changedLoopLengthMsg : Int -> msg
    , craftMsg : ContainerSort -> msg
    , nextBugIn : Int
    , hasPower : Bool
    , togglePowerMsg : msg
    }
    -> Game
    -> List (List ( String, Element msg ))
view { counter, hasPower, togglePowerMsg, money, shop, nextBugIn, scale, selected, loopLength, craftMsg, changedLoopLengthMsg, positionSelectedMsg, selectedMsg, buyMsg, sellMsg } { board, deck } =
    [ [ ( "Shop"
        , Shop.view
            { shop = shop
            , buyMsg = Just buyMsg
            , sellMsg = Just sellMsg
            , money = money
            , deck = deck
            }
        )
      , ( "Crafting", Crafting.view { craftMsg = craftMsg } )
      ]
    , List.singleton <|
        ( "Game"
        , Element.column Grid.section <|
            [ board
                |> BoardView.view
                    { scale = scale
                    , maybePositionMsg = Just positionSelectedMsg
                    }
            , deck |> DeckView.view scale (Just selectedMsg) selected
            ]
        )
    , [ ( "Machine"
        , Settings.view
            { changedLoopLengthMsg = changedLoopLengthMsg
            , loopLength = loopLength
            , hasPower = hasPower
            , togglePowerMsg = togglePowerMsg
            }
        )
      , ( "Details"
        , let
            price : Int
            price =
                (case selected of
                    Just (Crate item) ->
                        item

                    _ ->
                        Scrap
                )
                    |> (\item -> max 1 <| Data.maxPrice // ((shop |> Bag.count (item |> Item.itemToString)) + 1))
          in
          Details.view
            { amount =
                selected
                    |> Maybe.map
                        (\card ->
                            deck |> Bag.count (card |> FactoryCity.Data.CellType.containerSortToString)
                        )
                    |> Maybe.withDefault 0
            , selected = selected
            , sellMsg = sellMsg
            , price = price
            }
        )
      ]
    ]
