module FactoryCity.View.Game exposing (view, viewFinished, viewHome, viewReplay)

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
import FactoryCity.View.Deck as DeckView
import FactoryCity.View.Details as Details
import FactoryCity.View.Settings as Settings
import FactoryCity.View.Shop as Shop
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading
import Grid.Position exposing (Position)


viewFinished : Float -> Game -> Element msg
viewFinished scale { board, deck } =
    Element.column Grid.simple
        [ BoardView.view scale Nothing board
        , DeckView.view scale { sort = False } Nothing Nothing deck
        ]


viewReplay : Float -> Game -> Element msg
viewReplay scale { board, deck } =
    Element.column
        ([ Element.spacing (floor <| 5 * scale)
         , Background.color <| Element.rgb255 242 242 242
         , Element.padding (floor <| 20 * scale)
         , Border.rounded (floor <| 10 * scale)
         , Element.centerX
         ]
            |> ((::) <|
                    Element.inFront <|
                        Element.el
                            [ Element.width <| Element.fill
                            , Element.height <| Element.fill
                            , Background.color <| Element.rgb255 255 255 255
                            , Element.alpha 0.3
                            ]
                        <|
                            Element.el
                                (Heading.h1
                                    ++ [ Element.centerX
                                       , Element.centerY
                                       , Font.family
                                            [ Font.sansSerif ]
                                       ]
                                )
                            <|
                                Element.text "REPLAY"
               )
        )
    <|
        [ BoardView.view scale Nothing board
        , DeckView.view scale { sort = False } Nothing Nothing deck
        ]


viewHome : Float -> Game -> Element msg
viewHome scale { board, deck } =
    Element.column Grid.simple <|
        [ BoardView.view scale Nothing board
        , DeckView.view scale { sort = False } Nothing Nothing deck
        ]


view :
    Int
    -> Bag String
    ->
        { scale : Float
        , selected : Maybe ContainerSort
        , sort : Bool
        , loopLength : Int
        }
    ->
        { positionSelectedMsg : Position -> msg
        , selectedMsg : ContainerSort -> msg
        , buyMsg : String -> msg
        , sellMsg : ContainerSort -> msg
        , changedLoopLengthMsg : Int -> msg
        }
    -> Game
    -> Element msg
view money shop { scale, selected, sort, loopLength } { changedLoopLengthMsg, positionSelectedMsg, selectedMsg, buyMsg, sellMsg } { board, deck } =
    Element.wrappedRow Grid.spaceEvenly <|
        [ Shop.view
            { shop = shop
            , buyMsg = buyMsg
            , money = money
            }
        , Element.column Grid.simple <|
            [ BoardView.view scale (Just positionSelectedMsg) board
            , DeckView.view scale { sort = sort } (Just selectedMsg) selected deck
            ]
        , let
            price : Int
            price =
                selected
                    |> Maybe.map
                        (\card ->
                            max 1 <| Data.maxPrice // ((shop |> Bag.count (card |> FactoryCity.Data.CellType.containerSortToString)) + 1)
                        )
                    |> Maybe.withDefault 0
          in
          Element.column (Grid.simple ++ [ Element.width <| Element.shrink ]) <|
            [ Element.el Heading.h2 <| Element.text "Details"
            , Details.view
                { selected = selected
                , sellMsg = sellMsg
                , price = price
                }
            , Element.el Heading.h2 <| Element.text "Settings"
            , Settings.view
                { changedLoopLengthMsg = changedLoopLengthMsg
                , loopLength = loopLength
                }
            ]
        ]
