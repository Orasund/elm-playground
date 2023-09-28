module Square exposing (..)

import Piece exposing (Piece(..))


type alias Square =
    { piece : Piece
    , isWhite : Bool
    }


toString : Square -> String
toString square =
    case ( square.piece, square.isWhite ) of
        ( King, True ) ->
            "♔"

        ( King, False ) ->
            "♚"
