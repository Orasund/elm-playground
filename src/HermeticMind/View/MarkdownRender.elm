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


renderer : (String -> List (Html msg) -> Html msg) -> Renderer (Html msg)
renderer interactive =
    { heading =
        \{ level, children } ->
            children
                |> (let
                        attr =
                            []

                        h1 =
                            Html.h1
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 36), Css.fontFamilies [ "Dancing Script" ] ]
                                        ++ attr
                                ]

                        h2 =
                            Html.h2
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 24)
                                    , Css.margin4 (Css.px 16) (Css.px 0) (Css.px 8) (Css.px 0)
                                    , Css.fontFamilies [ "Dancing Script" ]
                                    ]
                                        ++ attr
                                ]

                        h3 =
                            Html.h3
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 18)
                                    , Css.margin4 (Css.px 18) (Css.px 0) (Css.px 8) (Css.px 0)
                                    ]
                                        ++ attr
                                ]

                        h4 =
                            Html.h4
                                [ Attributes.css <|
                                    [ Css.fontSize (Css.px 16)
                                    , Css.margin2 (Css.px 0) (Css.px 0)
                                    ]
                                        ++ attr
                                ]
                    in
                    case level of
                        Block.H1 ->
                            h1

                        Block.H2 ->
                            h2

                        Block.H3 ->
                            h3

                        Block.H4 ->
                            h4

                        Block.H5 ->
                            h4

                        Block.H6 ->
                            h4
                   )

    -- [Font.family [ Font.typeface "Dancing Script" ]]
    , paragraph =
        Html.p
            [ Attributes.css
                [ Css.textAlign Css.justify
                , Css.margin2 (Css.px 8) (Css.px 0)
                ]
            ]
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
            , Html.tag "interactive" interactive
                |> Html.withAttribute "name"
            , Html.tag "abstract" (Html.div [])
            , Html.tag "image"
                (\title height src _ ->
                    [ Html.img
                        ([ [ Attributes.src src
                           , Attributes.alt title
                           ]
                         , height
                            |> Maybe.map
                                (String.toInt
                                    >> Maybe.withDefault 0
                                    >> Attributes.height
                                    >> List.singleton
                                )
                            |> Maybe.withDefault []
                         ]
                            |> List.concat
                        )
                        []
                        |> List.singleton
                        |> Html.div
                            [ [ Css.displayFlex
                              , Css.justifyContent Css.center
                              ]
                                |> Attributes.css
                            ]
                    , title
                        |> Html.text
                        |> List.singleton
                        |> Html.div
                            [ [ Css.displayFlex
                              , Css.justifyContent Css.center
                              , Css.fontStyle Css.italic
                              ]
                                |> Attributes.css
                            ]
                    ]
                        |> Html.div
                            []
                )
                |> Html.withAttribute "title"
                |> Html.withOptionalAttribute "height"
                |> Html.withAttribute "src"
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
