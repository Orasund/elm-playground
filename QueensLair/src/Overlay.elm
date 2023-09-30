module Overlay exposing (..)

import Artefact exposing (Artefact)
import Piece exposing (Piece)


type Overlay
    = ShopOverlay { party : List Piece }
    | FoundArtefactOverlay Artefact
