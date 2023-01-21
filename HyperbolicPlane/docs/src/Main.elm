module Main exposing (main)

import Chapter
import Chapter.HyperbolicModels
import Chapter.IdealPoints
import Chapter.LineSegments
import ElmBook exposing (Book)
import ElmBook.ThemeOptions
import Html exposing (Html)


main : Book (Html msg)
main =
    ElmBook.book "Elm-Hyperbolic"
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.useHashBasedNavigation
            , ElmBook.ThemeOptions.backgroundGradient "#d8ffc2" "#aad498"
            , ElmBook.ThemeOptions.accent "#eb8139"
            , ElmBook.ThemeOptions.navAccent "#361c0a"
            , ElmBook.ThemeOptions.navAccentHighlight "#ff6800"
            ]
        |> ElmBook.withChapters
            [ Chapter.one
            , Chapter.HyperbolicModels.chapter
            , Chapter.IdealPoints.chapter
            , Chapter.LineSegments.chapter
            ]
