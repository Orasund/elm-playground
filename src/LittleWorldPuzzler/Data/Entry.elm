module LittleWorldPuzzler.Data.Entry exposing (Entry, decoder, encode, gameVersion, new)

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
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.Data.Game as Game exposing (EndCondition(..), Game)
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import LittleWorldPuzzler.View.Game as GameView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)
import UndoList.Encode as UndoListE


gameVersion : Int
gameVersion =
    2


type alias Entry =
    { history : UndoList Game
    , version : Int
    , score : Int
    }


new : UndoList Game -> Entry
new history =
    { history = history
    , version = gameVersion
    , score = history.present.score
    }



{------------------------
   Decoder
------------------------}


undoListDecoder : Decoder (UndoList Game)
undoListDecoder =
    D.map3 UndoList
        ((D.map (Maybe.withDefault []) << D.maybe << D.field "past") <| D.list Game.decoder)
        (D.field "present" Game.decoder)
        ((D.map (Maybe.withDefault []) << D.maybe << D.field "future") <| D.list Game.decoder)


decoder : Decoder Entry
decoder =
    D.map3
        (\history version score ->
            { history = history
            , version = version
            , score = score
            }
        )
        (D.field "history" <| undoListDecoder)
        (D.field "version" <| D.int)
        (D.field "score" <| D.int)



{------------------------
   Encoder
------------------------}


encode : Entry -> Value
encode { history, version, score } =
    E.object
        [ ( "history", UndoListE.undolist <| UndoList.map Game.encode <| history )
        , ( "version", E.int version )
        , ( "score", E.int score )
        ]
