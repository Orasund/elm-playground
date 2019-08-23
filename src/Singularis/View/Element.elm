module Singularis.View.Element exposing (black, menu, section, slider, subsection, title, white)

import Color
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Element.Input as Input
import Element.Region as Region
import Singularis.View as View exposing (maxScreenWidth)
import Markdown.Inline as Inline exposing (Inline)
import Markdown.Block as Block exposing (Block)

comfortaaFont : Font
comfortaaFont =
    Font.external
        { name = "Comfortaa"
        , url = "https://fonts.googleapis.com/css?family=Comfortaa&display=swap"
        }


black : Color
black =
    Element.fromRgb <| Color.toRgba <| Color.black


white : Color
white =
    Element.fromRgb <| Color.toRgba <| Color.white


menu : Float -> List { name : String, url : String } -> Element msg
menu scale =
    List.map
        (\{ name, url } ->
            Element.link
                [ Font.family <|
                    [ comfortaaFont
                    , Font.sansSerif
                    ]
                ]
            <|
                { url = url
                , label = Element.text <| name
                }
        )
        >> Element.row
            [ Element.width <| Element.px <| round <| (*) scale <| maxScreenWidth
            , Element.centerX
            , Element.spacing <| round <| (*) scale <| 20
            ]
        >> Element.el
            [ Background.color <| black
            , Font.color <| white
            , Element.width <| Element.fill
            , Element.padding <| round <| (*) scale <| 10
            ]


heading : Int -> String -> Element msg
heading size text =
    Element.el
        [ Font.size size
        , Font.family <|
            [ comfortaaFont
            , Font.sansSerif
            ]
        , Element.centerX
        ]
    <|
        Element.text text


slider : Float -> { onChange : Float -> msg, label : String, min : Float, max : Float, value : Float } -> Element msg
slider scale { onChange, label, min, max, value } =
    Element.row [ Element.width <| Element.fill, Element.spacing <| round <| (*) scale <| 10 ] <|
        [ Element.text <| label
        , Input.slider
            [ Element.behindContent <|
                Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color black
                    ]
                <|
                    Element.none
            , Element.width <| Element.fill
            ]
            { onChange = onChange
            , label =
                Input.labelLeft [] <|
                    Element.text <|String.fromFloat <|
                        (toFloat <| truncate <| 10 * value)
                            / 10
            , min = min
            , max = max
            , value = value
            , thumb = Input.defaultThumb
            , step = Nothing
            }
        ]


title : Float -> String -> Element msg
title scale =
    heading <| round <| (*) scale <| 90


section : Float -> String -> Element msg
section scale =
    heading <| round <| (*) scale <| 45


subsection : Float -> String -> Element msg
subsection scale =
    heading <| round <| (*) scale <| 30

fromInlineMarkdown : Inline i -> Element msg
fromInlineMarkdown inline =
    case inline of
        Text str ->
            Element.text str

        HardLineBreak ->
            Element.el [Element.width Element.fill] <|
                Element.none

        CodeInline codeStr ->
            Element.paragraph
                [ Font.family
                    [Font.monospace]
                ] <|
                    Element.text codeStr

        Link url maybeTitle inlines ->
                    Element.link
                        (maybeTitle
                            |> Maybe.map (Region.description>>List.singleton)
                            |> Maybe.withDefault []
                        )
                        {url = url
                        ,label = fromInlineMarkdown inline
                        }

        Image url maybeTitle _ ->
                Element.image []
                <| { src : url
                    , description : maybeTitle |> Maybe.withDefault ""
                    }

        HtmlInline _ _ inlines ->
            fromInlineMarkdown inline

        Emphasis length inlines ->
            case length of
                1 ->
                    Element.el[Font.italic]<|
                        fromInlineMarkdown inline

                2 ->
                    Element.el[Font.bold]<|
                        fromInlineMarkdown inline

                _ ->
                    fromInlineMarkdown inline

        Custom _ inlines ->
            fromInlineMarkdown inline

fromMarkdown : Float -> Block b i -> List (Html msg)
fromMarkdown scale block =
    case block of
        BlankLine _ ->
            []

        Heading _ level inlines ->
                    case level of
                        1 ->
                            title scale <| Inline.extractText inlines

                        2 ->
                            section scale <| (Inline.extractText inlines)

                        3 ->
                            subsection scale  <| (Inline.extractText inlines)

                        _ ->
                            fromInlineMarkdown inlines

        ThematicBreak ->
            Element.el [Element.width Element.fill] <| Element.none

        Paragraph _ inlines ->
            Element.paragraph [] <|
                
                (List.map fromInlineMarkdown inlines)

        CodeBlock _ codeStr ->
            Element.paragraph [Font.family <| List.singleton Font.monospace] <|
                Element.text codeStr

        BlockQuote blocks ->
            Element.paragraph [Font.italic] <|
                fromMarkdown blocks

        List model items ->
            List.map
                (List.map Block.blockToHtml
                    >> List.concat
                    >> Html.li []
                )
                items
                |> (case model.type_ of
                        Ordered startInt ->
                            if startInt == 1 then
                                Html.ol []
                            else
                                Html.ol [ start startInt ]

                        Unordered ->
                            Html.ul []
                   )
                |> (\a -> (::) a [])
                |> Element.html

        PlainInlines inlines ->
            fromInlineMarkdown inlines

        Custom customBlock blocks ->
            fromMarkdown blocks