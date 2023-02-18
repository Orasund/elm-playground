module Chapter.Card exposing (..)

import ElmBook.Chapter exposing (Chapter)
import View.Card
import View.Component


chapter : Chapter state
chapter =
    ElmBook.Chapter.chapter "Game.Card"
        |> ElmBook.Chapter.withComponentList
            [ ( "Styles"
              , View.Component.list
                    [ ( "empty", View.Card.empty )
                    , ( "default", View.Card.default )
                    ]
              )
            , ( "Ratios"
              , View.Component.list
                    [ ( "ratio (2/3)", View.Card.default )
                    , ( "ratio 1", View.Card.square )
                    , ( "ratio (3/2)", View.Card.horizontal )
                    ]
              )
            , ( "Layouts"
              , View.Component.list
                    [ ( "header", View.Card.titleRow )
                    , ( "fillingImage", View.Card.fullImage )
                    , ( "description", View.Card.imageAndDesc )
                    ]
              )
            , ( "Transformations"
              , View.Component.list
                    [ ( "rotate (pi/2)", View.Card.rotated )
                    , ( "scale (1/2)", View.Card.small )
                    , ( "translate (0,-50)", View.Card.drawn )
                    ]
              )
            ]
        |> ElmBook.Chapter.renderWithComponentList
            ""
