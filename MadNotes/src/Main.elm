module Main exposing (main)

import Chapter.AnatomyOfDeath
import Chapter.DangerousColors
import Chapter.Darkness
import Chapter.Introduction
import Chapter.TheWatchers
import Data.Shared exposing (State)
import ElmBook exposing (Book)
import ElmBook.Actions as Actions
import ElmBook.ComponentOptions as ComponentOptions
import ElmBook.StatefulOptions as StatefulOptions
import ElmBook.ThemeOptions as ThemeOptions
import Html
import Random
import Time
import View.Chapter


main : Book State
main =
    let
        chapters =
            [ Chapter.Introduction.chapter
            , Chapter.AnatomyOfDeath.chapter
            , Chapter.DangerousColors.chapter
            , Chapter.TheWatchers.chapter
            , Chapter.Darkness.chapter
            ]
    in
    ElmBook.book "Abschrift des Manuskripts der Totenrufe von Kitab al-Azif"
        |> ElmBook.withStatefulOptions
            [ StatefulOptions.initialState (chapters |> Data.Shared.init)
            , StatefulOptions.subscriptions
                [ Time.every 60000
                    (\posix ->
                        posix
                            |> Time.posixToMillis
                            |> Random.initialSeed
                            |> Actions.updateStateWith Data.Shared.initialSeed
                    )
                ]
            ]
        |> ElmBook.withComponentOptions [ ComponentOptions.displayBlock ]
        |> ElmBook.withThemeOptions
            [ ThemeOptions.preferDarkMode
            , ThemeOptions.useHashBasedNavigation
            , ThemeOptions.backgroundGradient "#64215a" "#3d3d3d"
            , ThemeOptions.logo (Html.span [] [])
            , ThemeOptions.globals
                [ Html.node "style"
                    []
                    [ ".elm-book-action-log-preview-empty-wrapper,"
                        ++ ".elm-book--wrapper--menu--footer{"
                        ++ "display:none"
                        ++ "}\n\n"
                        ++ "em {"
                        ++ "font-style:normal;"
                        ++ "font-size: 0.95em"
                        ++ "}\n\n"
                        ++ ".elm-book-dark-mode .elm-book-md img {"
                        ++ "background-color: #3b3f47"
                        ++ "}\n\n"
                        ++ ".elm-book-md img {"
                        ++ "background-color: #f0f0f0;"
                        ++ "padding:24px"
                        ++ "}"
                        |> Html.text
                    ]
                ]
            ]
        |> ElmBook.withChapters
            (chapters |> List.map (\chapter -> View.Chapter.view chapter.title chapter.pages))
