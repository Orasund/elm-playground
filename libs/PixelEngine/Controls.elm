module PixelEngine.Controls exposing (Input, basic, defaultLayout,custom, supportingMobile)

import Char
import Keyboard
import PixelEngine.Graphics as Graphics exposing (Options)
import PixelEngine.Graphics.Abstract as Abstract
import Window


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


supportingMobile : { windowSize : Window.Size } -> Options msg -> Options msg
supportingMobile { windowSize } (Abstract.Options options) =
    Abstract.Options { options | controllerOptions = Just { windowSize = windowSize } }


defaultLayout : Char -> Input
defaultLayout =
    \char ->
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


custom : (Char -> Input) -> (Input -> msg) -> Sub msg
custom toInput fun =
    Keyboard.presses <|
        Char.fromCode
            >> toInput
            >> fun


basic : (Input -> msg) -> Sub msg
basic fun =
    Keyboard.presses <|
        Char.fromCode
            >> defaultLayout
            >> fun
