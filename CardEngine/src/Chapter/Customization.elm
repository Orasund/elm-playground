module Chapter.Customization exposing (..)

import ElmBook.Chapter exposing (Chapter)


chapter : Chapter msg
chapter =
    ElmBook.Chapter.chapter "No magic included"
        |> ElmBook.Chapter.render """
Please feel free to look at the source code. We tried to make the code as clear as possible.

While developing this package we followed the following rules:

* We always used `height` and `aspect-ratio` instead of using `weight`.
* We used `rgba(0, 0, 0, 0.2)` as a placeholder for color.
* The styling is intended to be overwritten.
"""
