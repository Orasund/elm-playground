module FactoryCity.View.Game exposing (view, viewFinished, viewHome, viewReplay)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FactoryCity.Data.CellType exposing (CellType)
import FactoryCity.Data.Deck exposing (Selected(..))
import FactoryCity.Data.Game exposing (EndCondition(..), Game)
import FactoryCity.View.Board as BoardView
import FactoryCity.View.Deck as DeckView
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


view : { scale : Float, selected : Maybe Selected, sort : Bool } -> { positionSelectedMsg : Position -> msg, selectedMsg : Selected -> msg } -> Game -> Element msg
view { scale, selected, sort } { positionSelectedMsg, selectedMsg } { board, deck } =
    Element.column Grid.simple <|
        [ BoardView.view scale (Just positionSelectedMsg) board
        , DeckView.view scale { sort = sort } (Just selectedMsg) selected deck
        ]
