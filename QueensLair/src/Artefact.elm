module Artefact exposing (..)


type Artefact
    = EscapeRope
    | PoliceBox
    | Coconuts


list : List Artefact
list =
    [ EscapeRope, PoliceBox, Coconuts ]


name : Artefact -> String
name item =
    case item of
        EscapeRope ->
            "Escape Rope"

        PoliceBox ->
            "Police Box"

        Coconuts ->
            "Coconuts"


description : Artefact -> String
description item =
    case item of
        EscapeRope ->
            "Restart the level"

        PoliceBox ->
            "Undo your last move"

        Coconuts ->
            "Move a piece like a knight"
