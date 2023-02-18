module Chapter.Drag exposing (..)

import ElmBook.Chapter exposing (Chapter)


type alias State =
    { left : List (), right : List () }


chapter : Chapter state
chapter =
    ElmBook.Chapter.chapter "Drag & Drop"
        |> ElmBook.Chapter.renderStatefulComponentList
            []
