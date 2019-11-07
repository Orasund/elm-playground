module UiFramework.Example exposing (main)

import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Framework
import Framework.Card as Card
import Framework.Color as Color
import Framework.Heading as Heading
import Framework.Section as Section
import Html exposing (Html)


heading : Element msg
heading =
    Element.column Section.simple <|
        [ Element.el Heading.h1 <| Element.text "Heading"
        , Element.column Card.fill
            [ Element.el Heading.h1 <| Element.text "Heading.h1"
            , Element.el Heading.h2 <| Element.text "Heading.h2"
            , Element.el Heading.h3 <| Element.text "Heading.h3"
            , Element.el Heading.h3 <| Element.text "Heading.h4"
            , Element.el Heading.h3 <| Element.text "Heading.h5"
            , Element.el Heading.h3 <| Element.text "Heading.h6"
            ]
        ]


card : Element msg
card =
    Element.column Section.simple <|
        [ Element.el Heading.h1 <| Element.text "Card"
        , Element.wrappedRow Card.fill
            [ Element.el Card.basic <| Element.text "Card.basic"
            , Element.el Card.small <| Element.text "Card.small"
            , Element.el Card.large <| Element.text "Card.large"
            , Element.el Card.fill <| Element.text "Card.fill"
            ]
        ]


color : Element msg
color =
    Element.column Section.simple <|
        [ Element.el Heading.h1 <| Element.text "Color"
        , Element.column Card.fill
            [ Element.el (Card.fill ++ Color.primary) <| Element.text "Color.primary"
            , Element.el (Card.fill ++ Color.info) <| Element.text "Color.info"
            , Element.el (Card.fill ++ Color.success) <| Element.text "Color.success"
            , Element.el (Card.fill ++ Color.warning) <| Element.text "Color.warning"
            , Element.el (Card.fill ++ Color.danger) <| Element.text "Color.danger"
            , Element.el (Card.fill ++ Color.light) <| Element.text "Color.light"
            , Element.el (Card.fill ++ Color.dark) <| Element.text "Color.dark"
            ]
        , Element.column Card.fill
            [ Element.el Heading.h2 <| Element.text "Elm-Ui Colors"
            , Element.el (Card.fill ++ [ Background.color Color.turquoise ]) <| Element.text "Color.turquoise"
            , Element.el (Card.fill ++ [ Background.color Color.cyan ]) <| Element.text "Color.cyan"
            , Element.el (Card.fill ++ [ Background.color Color.green ]) <| Element.text "Color.green"
            , Element.el (Card.fill ++ [ Background.color Color.yellow ]) <| Element.text "Color.yellow"
            , Element.el (Card.fill ++ [ Background.color Color.red ]) <| Element.text "Color.red"
            , Element.el (Card.fill ++ [ Background.color Color.lighterGrey ]) <| Element.text "Color.lighterGrey"
            , Element.el (Card.fill ++ [ Background.color Color.lightGrey ]) <| Element.text "Color.lightGrey"
            , Element.el (Card.fill ++ [ Background.color Color.grey ]) <| Element.text "Color.grey"
            , Element.el (Card.fill ++ [ Background.color Color.darkGrey, Font.color <| Element.rgb255 255 255 255 ]) <| Element.text "Color.darkGrey"
            , Element.el (Card.fill ++ [ Background.color Color.darkerGrey, Font.color <| Element.rgb255 255 255 255 ]) <| Element.text "Color.darkerGrey"
            ]
        ]


view : Element msg
view =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.width (Element.fill |> Element.maximum 1200)
        , Element.padding <| 20
        ]
        [ heading
        , color
        , card
        ]


main : Html msg
main =
    Framework.layout [] <|
        view
