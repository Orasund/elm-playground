module Chapter.Customization exposing (..)

import ElmBook.Chapter exposing (Chapter)


chapter : Chapter msg
chapter =
    ElmBook.Chapter.chapter "Customization"
        |> ElmBook.Chapter.render """
While developing this package we followed the following rules:

* We always used `height` and `aspect-ratio` instead of using `weight`.
* We used `rgba(0, 0, 0, 0.2)` as a placeholder for color.
"""
