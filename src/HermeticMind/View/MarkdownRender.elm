module HermeticMind.View.MarkdownRender exposing (renderer)

import Css
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html as UnstyledHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Markdown.Block as Block exposing (Block, HeadingLevel(..), Inline, ListItem(..), Task(..))
import Markdown.Html as Html
import Markdown.Renderer exposing (Renderer)


renderer : Renderer (Html msg)
renderer =
    { heading =
        \{ level, children } ->
            children
                |> (let
                        attr =
                            [ Css.fontFamilies [ "Dancing Script" ] ]
                    in
                    case level of
                        Block.H1 ->
                            Html.h1
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 36) ]
                                        ++ attr
                                ]

                        Block.H2 ->
                            Html.h2
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 20) ]
                                        ++ attr
                                ]

                        Block.H3 ->
                            Html.h3
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 16) ]
                                        ++ attr
                                ]

                        Block.H4 ->
                            Html.h4 [ Attributes.css <| [ Css.fontSize (Css.px 14) ] ++ attr ]

                        Block.H5 ->
                            Html.h5 [ Attributes.css <| [ Css.fontSize (Css.px 14) ] ++ attr ]

                        Block.H6 ->
                            Html.h6
                                [ Attributes.css <| [ Css.fontSize (Css.px 14) ] ++ attr ]
                   )

    -- [Font.family [ Font.typeface "Dancing Script" ]]
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attributes.href link.destination
                        , Attributes.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attributes.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attributes.src imageInfo.src
                        , Attributes.alt imageInfo.alt
                        , Attributes.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attributes.src imageInfo.src
                        , Attributes.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attributes.disabled True
                                                        , Attributes.checked False
                                                        , Attributes.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attributes.disabled True
                                                        , Attributes.checked True
                                                        , Attributes.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attributes.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html =
        Html.oneOf
            [ Html.tag "page"
                (Html.div
                    [ Attributes.css
                        [ Css.padding <| Css.px 50
                        , Css.property "page-break-after" "always"
                        , Css.property "page-break-before" "always"
                        ]
                    ]
                )
            ]
    , codeBlock =
        \{ body, language } ->
            Html.pre []
                [ Html.code []
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attributes.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell = \maybeAlignment -> Html.td []
    , strikethrough = Html.s []
    }
