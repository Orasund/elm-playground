module Main exposing (..)

import Chapter.Area
import Chapter.Card
import Chapter.Customization
import Chapter.Stack
import ElmBook exposing (Book)
import ElmBook.StatefulOptions
import ElmBook.ThemeOptions


type alias Model =
    { area : Chapter.Area.State
    }


init : Model
init =
    { area = Chapter.Area.init }


main : Book Model
main =
    ElmBook.book "Elm-Card-Game"
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.useHashBasedNavigation ]
        |> ElmBook.withStatefulOptions
            [ ElmBook.StatefulOptions.initialState init ]
        |> ElmBook.withChapters
            [ Chapter.Customization.chapter
            , Chapter.Card.chapter
            , Chapter.Stack.chapter
            , Chapter.Area.chapter { get = .area, setTo = \model state -> { model | area = state } }
            ]
