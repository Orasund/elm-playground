module Chapter.Stack exposing (..)

import ElmBook.Chapter exposing (Chapter)
import View.Component
import View.Stack


chapter : Chapter msg
chapter =
    ElmBook.Chapter.chapter "Stack"
        |> ElmBook.Chapter.withComponentList
            [ ( "Stack"
              , View.Component.list
                    [ ( "Single Card", View.Stack.singleCard )
                    , ( "withMovement", View.Stack.below )
                    , ( "withRotation", View.Stack.rotated )
                    , ( "randomRotation and randomMovement", View.Stack.random )
                    , ( "withRotation and withMovement", View.Stack.hand )
                    ]
              )
            ]
        |> ElmBook.Chapter.renderWithComponentList ""
