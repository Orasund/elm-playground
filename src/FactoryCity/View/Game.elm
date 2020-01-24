module FactoryCity.View.Game exposing (view)

import Bag exposing (Bag)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FactoryCity.Data as Data
import FactoryCity.Data.CellType exposing (CellType, ContainerSort)
import FactoryCity.Data.Deck
import FactoryCity.Data.Game exposing (EndCondition(..), Game)
import FactoryCity.View.Board as BoardView
import FactoryCity.View.Crafting as Crafting
import FactoryCity.View.Deck as DeckView
import FactoryCity.View.Details as Details
import FactoryCity.View.Settings as Settings
import FactoryCity.View.Shop as Shop
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading
import Grid.Position exposing (Position)
import Html.Attributes as Attributes


view :
    { counter : Int
    , money : Int
    , shop : Bag String
    , scale : Float
    , selected : Maybe ContainerSort
    , sort : Bool
    , loopLength : Int
    , positionSelectedMsg : Position -> msg
    , selectedMsg : ContainerSort -> msg
    , buyMsg : String -> msg
    , sellMsg : ContainerSort -> msg
    , changedLoopLengthMsg : Int -> msg
    , craftMsg : ContainerSort -> msg
    , nextBugIn : Int
    }
    -> Game
    -> List (List ( String, Element msg ))
view { counter, money, shop, nextBugIn, scale, selected, sort, loopLength, craftMsg, changedLoopLengthMsg, positionSelectedMsg, selectedMsg, buyMsg, sellMsg } { board, deck } =
    [ [ ( "Shop"
        , Shop.view
            { shop = shop
            , buyMsg = Just buyMsg
            , money = money
            }
        )
      , ( "Crafting", Crafting.view { craftMsg = craftMsg } )
      ]
    , List.singleton <|
        ( "Game"
        , Element.column Grid.section <|
            [ Element.row Grid.simple <|
                [ Element.el [ Element.width <| Element.fill ] <| Element.none
                , Element.el (Heading.h1 ++ [ Element.width <| Element.fill ]) <|
                    Element.el [ Element.centerX ] <|
                        Element.text <|
                            String.fromInt <|
                                counter
                , Element.paragraph [ Element.width <| Element.fill ] <|
                    List.singleton <|
                        Element.text <|
                            "Next bug in "
                                ++ (String.fromInt <| nextBugIn)
                                ++ " turns"
                ]
            , board
                |> BoardView.view
                    { scale = scale
                    , maybePositionMsg = Just positionSelectedMsg
                    }
            , deck |> DeckView.view scale { sort = sort } (Just selectedMsg) selected
            ]
        )
    , [ ( "Details"
        , let
            price : ContainerSort -> Int
            price c =
                selected
                    |> Maybe.map
                        (\card ->
                            max 1 <| Data.maxPrice // ((shop |> Bag.count (c |> FactoryCity.Data.CellType.containerSortToString)) + 1)
                        )
                    |> Maybe.withDefault 0
          in
          Details.view
            { selected = selected
            , sellMsg = sellMsg
            , price = price
            }
        )
      , ( "Settings"
        , Settings.view
            { changedLoopLengthMsg = changedLoopLengthMsg
            , loopLength = loopLength
            }
        )
      ]
    ]
