module View.Card exposing (..)

import CardGame.Card
import Html exposing (Attribute, Html)
import Html.Attributes
import View.Component


empty : Html msg
empty =
    CardGame.Card.empty "No Card"


default : Html msg
default =
    Html.text "Title"
        |> CardGame.Card.title []
        |> List.singleton
        |> CardGame.Card.card []


square : Html msg
square =
    Html.text "Title"
        |> CardGame.Card.title []
        |> List.singleton
        |> CardGame.Card.card [ CardGame.Card.ratio 1 ]


horizontal : Html msg
horizontal =
    Html.text "Title"
        |> CardGame.Card.title []
        |> List.singleton
        |> CardGame.Card.card [ CardGame.Card.ratio 1.5 ]


titleRow : Html msg
titleRow =
    [ Html.div [] [ Html.text "Title with a symbol" ]
    , Html.div [] [ Html.text "🔥" ]
    ]
        |> CardGame.Card.header []
        |> List.singleton
        |> CardGame.Card.card []


fullImage : Html msg
fullImage =
    [ Html.text "Card with an Image" |> CardGame.Card.title []
    , View.Component.image |> CardGame.Card.fillingImage []
    ]
        |> CardGame.Card.card []


imageAndDesc : Html msg
imageAndDesc =
    [ Html.text "Title" |> CardGame.Card.title []
    , View.Component.image |> CardGame.Card.fillingImage []
    , Html.text "Card with image and description"
        |> CardGame.Card.description []
    ]
        |> CardGame.Card.card []


rotated : Html msg
rotated =
    View.Component.defaultCard
        [ CardGame.Card.transform
            [ CardGame.Card.rotate (pi / 2) ]
        ]


small : Html msg
small =
    View.Component.defaultCard
        [ CardGame.Card.transform
            [ CardGame.Card.zoom (1 / 2) ]
        ]


drawn : Html msg
drawn =
    View.Component.defaultCard
        [ CardGame.Card.transform
            [ CardGame.Card.move ( 0, -50 )
            ]
        ]
