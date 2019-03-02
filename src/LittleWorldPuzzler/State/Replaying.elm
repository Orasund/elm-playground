module LittleWorldPuzzler.State.Replaying exposing (Model, Msg, update, view)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Framework.Modifier exposing (Modifier(..))
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import LittleWorldPuzzler.Data.Game exposing (Game)
import LittleWorldPuzzler.State as State exposing (Action(..))
import LittleWorldPuzzler.View.Button as Button
import LittleWorldPuzzler.View.Game as GameView
import LittleWorldPuzzler.View.Header as HeaderView
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


update : Msg -> Model -> Action Model Msg Never
update msg model =
    case msg of
        Next ->
            Update
                ( model |> UndoList.redo, Cmd.none )

        Previous ->
            Update
                ( model |> UndoList.undo, Cmd.none )



----------------------
-- View
----------------------


view : Float -> msg -> (Msg -> msg) -> Model -> Element msg
view scale restartMsg msgMapper model =
    let
        ({ score } as game) =
            model.present
    in
    Element.column
        [ Element.centerY
        , Element.centerX
        , Element.spacing 5
        ]
        [ HeaderView.viewWithUndo scale
            { restartMsg = restartMsg
            , previousMsg = msgMapper Previous
            , nextMsg = msgMapper Next
            }
            score
        , GameView.viewReplay scale game
        ]
