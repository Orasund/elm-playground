module LittleWorldPuzzler.State.Replaying exposing (Model, Msg, update, view)

import Action
import Element exposing (Element)
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))
import LittleWorldPuzzler.Data.Deck exposing (Selected(..))
import LittleWorldPuzzler.Data.Game exposing (Game)
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


type alias Action =
    Action.Action Model Never Never Never



----------------------
-- Update
----------------------


update : Msg -> Model -> Action
update msg model =
    case msg of
        Next ->
            Action.updating
                ( model |> UndoList.redo, Cmd.none )

        Previous ->
            Action.updating
                ( model |> UndoList.undo, Cmd.none )



----------------------
-- View
----------------------


view :
    Float
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper model =
    let
        ({ score } as game) =
            model.present
    in
    ( Nothing
    , [ HeaderView.viewWithUndo scale
            { restartMsg = restartMsg
            , previousMsg = msgMapper Previous
            , nextMsg = msgMapper Next
            }
            score
      , GameView.viewReplay scale game
      ]
    )
