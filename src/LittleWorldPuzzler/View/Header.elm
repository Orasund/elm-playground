module LittleWorldPuzzler.View.Header exposing (view)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Framework.Modifier as Modifier exposing (Modifier(..))
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Position)
import Html exposing (Html)
import Http exposing (Body, Error(..), Expect)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.Data.Entry as Entry exposing (Entry, gameVersion)
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.Request as Request exposing (Response(..))
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import LittleWorldPuzzler.View.Game as GameView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)


view : Float -> msg -> Int -> Element msg
view scale restartMsg score =
    Element.row
        [ Element.spaceEvenly
        , Element.centerX
        , Element.width <| Element.px <| floor <| 608 * scale
        ]
    <|
        [ Element.el
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.alignBottom
            , Font.size <| floor <| 20 * scale
            , Font.color <| Element.rgb255 255 255 255
            , Font.family
                [ Font.sansSerif ]
            ]
          <|
            Element.text ("Version " ++ String.fromInt gameVersion ++ ": Sea Update")
        , Element.el [ Font.size <| floor <| 50 * scale ] <|
            Element.text <|
                String.fromInt score
        , Button.view
            [ Element.width <| Element.px <| floor <| 150 * scale
            , Element.padding <| floor <| 7 * scale
            , Border.rounded (floor <| 10 * scale)
            , Font.size <| floor <| 36 * scale
            , Font.family
                [ Font.sansSerif ]
            ]
          <|
            { onPress = Just restartMsg
            , label = Element.text "Restart"
            }
        ]
