module Main exposing (..)

import Chapter.Card
import Chapter.Customization
import Chapter.Stack
import ElmBook exposing (Book)
import ElmBook.ThemeOptions


type alias State =
    {}


main : Book State
main =
    ElmBook.book "Elm-Card-Engine"
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.useHashBasedNavigation ]
        |> ElmBook.withChapters
            [ Chapter.Customization.chapter
            , Chapter.Card.chapter
            , Chapter.Stack.chapter
            , Chapter.Drag.chapter
            ]
