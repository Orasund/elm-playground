module Action exposing (..)

import Artefact exposing (Artefact(..))
import Piece exposing (Piece(..))


type Action
    = ResetLevel
    | NextLevel (List Piece)
    | RemoveArtefactAnd Artefact Action
    | AddArtefactAnd Artefact Action
    | OverrideMovement Piece
    | UndoMove
    | FindArtefact
    | EndMove


fromArtefact : Artefact -> Action
fromArtefact artefact =
    case artefact of
        EscapeRope ->
            ResetLevel

        PoliceBox ->
            UndoMove

        Coconuts ->
            OverrideMovement Knight
