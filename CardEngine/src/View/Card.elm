module View.Card exposing (..)

import Game.Card
import Html exposing (Html)
import View.Component


empty : Html msg
empty =
    View.Component.empty []


back : Html msg
back =
    Html.text "Title" |> Game.Card.back []


default : Html msg
default =
    Html.text "Title"
        |> Game.Card.title []
        |> List.singleton
        |> Game.Card.card []


square : Html msg
square =
    Html.text "Title"
        |> Game.Card.title []
        |> List.singleton
        |> Game.Card.card [ Game.Card.ratio 1 ]


horizontal : Html msg
horizontal =
    Html.text "Title"
        |> Game.Card.title []
        |> List.singleton
        |> Game.Card.card [ Game.Card.ratio 1.5 ]


titleRow : Html msg
titleRow =
    [ Html.div [] [ Html.text "Title with a symbol" ]
    , Html.div [] [ Html.text "🔥" ]
    ]
        |> Game.Card.header []
        |> List.singleton
        |> Game.Card.card []


fullImage : Html msg
fullImage =
    [ Html.text "Card with an Image" |> Game.Card.title []
    , View.Component.image |> Game.Card.fillingImage []
    ]
        |> Game.Card.card []


imageAndDesc : Html msg
imageAndDesc =
    [ Html.text "Title" |> Game.Card.title []
    , View.Component.image |> Game.Card.fillingImage []
    , Html.text "Card with image and description"
        |> Game.Card.description []
    ]
        |> Game.Card.card []


rotated : Html msg
rotated =
    View.Component.defaultCard
        [ Game.Card.transform
            [ Game.Card.rotate (pi / 2) ]
        ]


small : Html msg
small =
    View.Component.defaultCard
        [ Game.Card.transform
            [ Game.Card.zoom (1 / 2) ]
        ]


drawn : Html msg
drawn =
    View.Component.defaultCard
        [ Game.Card.transform
            [ Game.Card.move ( 0, -50 )
            ]
        ]
