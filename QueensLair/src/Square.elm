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

        ( Rook, True ) ->
            "♖"

        ( Rook, False ) ->
            "♜"

        ( Bishop, True ) ->
            "♗"

        ( Bishop, False ) ->
            "♝"

        ( Knight, True ) ->
            "♘"

        ( Knight, False ) ->
            "♞"

        ( Pawn, True ) ->
            "♙"

        ( Pawn, False ) ->
            "♟"
