module LittleWorldPuzzler.State.Replaying exposing (Model, Msg, update, view)

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
import LittleWorldPuzzler.Automata as Automata
import LittleWorldPuzzler.Data.Board as Board exposing (Board)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck as Deck exposing (Deck, Selected(..))
import LittleWorldPuzzler.Data.Game as Game exposing (Game)
import LittleWorldPuzzler.View.Board as BoardView
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Deck as DeckView
import LittleWorldPuzzler.View.Game as GameView
import Process
import Random exposing (Generator, Seed)
import Task
import UndoList exposing (UndoList)



----------------------
-- Model
----------------------


type alias Model =
    UndoList Game


type Msg
    = Next
    | Previous



----------------------
-- Update
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( model |> UndoList.redo, Cmd.none )

        Previous ->
            ( model |> UndoList.undo, Cmd.none )



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper model =
    let
        ({ board, deck, score } as game) =
            model.present
    in
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.spaceEvenly
            , Element.centerX
            , Element.width <| Element.px <| floor <| 608 * scale
            ]
          <|
            [ Element.row
                [ Element.width <| Element.px <| floor <| 150 * scale
                , Element.spacing 5
                ]
              <|
                [ Button.view
                    [ Element.padding <| floor <| 7 * scale
                    , Border.rounded (floor <| 10 * scale)
                    , Font.size <| floor <| 36 * scale
                    , Element.width <| Element.px <|floor <| 36 * scale
                    , Font.family
                        [ Font.sansSerif ]
                    ]
                    { onPress = Just (msgMapper Previous)
                    , label = Element.text "<"
                    }
                , Button.view
                    [ Element.padding <| floor <| 7 * scale
                    , Border.rounded (floor <| 10 * scale)
                    , Element.width <| Element.px <|floor <| 36 * scale
                    , Font.size <| floor <| 36 * scale
                    , Font.family
                        [ Font.sansSerif ]
                    ]
                    { onPress = Just (msgMapper Next)
                    , label = Element.text ">"
                    }
                ]
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
                { onPress = Just restartMsg
                , label = Element.text "Restart"
                }
            ]
        , GameView.view
            { scale = scale
            , selected = Nothing
            , restartMsg = restartMsg
            , status = Nothing
            , highscore = Nothing
            }
            Nothing
            game
        ]
