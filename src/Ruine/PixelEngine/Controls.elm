module PixelEngine.Controls exposing (Input)

import Char
import Keyboard


type Input
    = Left
    | Right
    | Up
    | Down
    | A
    | B
    | X
    | Y
    | None


custom : { up : Char, down : Char, left : Char, right : Char, a : Char, b : Char
    , x : Char
    , y : Char
    } -> (Input -> msg) -> Sub msg
custom {up,down,left,right,a,b,x,y} fun =
    Keyboard.presses <|
        Char.fromCode
          >> (\char ->
            if char == up then
              Up
            else if char == down then
              Down
            else if char == left then
              Left
            else if char == right then
              Right
            else if char == a then
              A
            else if char == b then
              B
            else if char == x then
              X
            else if char == y then
              Y
            else
              None
          )
          >> fun

basic : (Input -> msg) -> Sub msg
basic fun =
    Keyboard.presses <|
        Char.fromCode
            >> (\char ->
                    case char of
                        'w' ->
                            Up

                        'W' ->
                            Up

                        's' ->
                            Down

                        'S' ->
                            Down

                        'd' ->
                            Right

                        'D' ->
                            Right

                        'a' ->
                            Left

                        'A' ->
                            Left

                        ' ' ->
                            A

                        'q' ->
                            X

                        'Q' ->
                            X

                        'e' ->
                            Y

                        'E' ->
                            Y

                        'x' ->
                            B

                        'X' ->
                            B

                        _ ->
                            None
               )
            >> fun
