module Singularis.View.Element exposing
    ( black
    , fromMarkdown
    , menu
    , section
    , slider
    , spectralFont
    , subsection
    , title
    , white
    )

import Color
import Dict exposing (Dict)
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes as Attributes
import Markdown.Block as Block exposing (Block(..), ListType(..))
import Markdown.Inline as Inline exposing (Inline(..))
import Singularis.View as View exposing (maxScreenWidth)


comfortaaFont : Font
comfortaaFont =
    Font.external
        { name = "Comfortaa"
        , url = "https://fonts.googleapis.com/css?family=Comfortaa&display=swap"
        }


spectralFont : Font
spectralFont =
    Font.external
        { name = "Spectral"
        , url = "https://fonts.googleapis.com/css?family=Spectral&display=swap"
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
                    Element.text <|
                        String.fromFloat <|
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
    heading <| round <| (*) scale <| 75


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
            Element.paragraph [] <| List.singleton <| Element.text str

        HardLineBreak ->
            Element.el [ Element.width Element.fill ] <|
                Element.none

        CodeInline codeStr ->
            Element.paragraph
                [ Font.family
                    [ Font.monospace ]
                ]
            <|
                List.singleton <|
                    Element.text codeStr

        Link url maybeTitle inlines ->
            Element.link
                (maybeTitle
                    |> Maybe.map (Region.description >> List.singleton)
                    |> Maybe.withDefault []
                )
                { url = url
                , label =
                    Element.column [] <|
                        List.map fromInlineMarkdown <|
                            inlines
                }

        Image url maybeTitle _ ->
            Element.image [] <|
                { src = url
                , description = maybeTitle |> Maybe.withDefault ""
                }

        HtmlInline _ _ inlines ->
            Element.column [] <|
                List.map fromInlineMarkdown <|
                    inlines

        Emphasis length inlines ->
            case length of
                1 ->
                    Element.el [ Font.italic ] <|
                        Element.column [] <|
                            List.map fromInlineMarkdown <|
                                inlines

                2 ->
                    Element.el [ Font.bold ] <|
                        Element.column [] <|
                            List.map fromInlineMarkdown <|
                                inlines

                _ ->
                    Element.column [] <|
                        List.map fromInlineMarkdown <|
                            inlines

        Inline.Custom _ inlines ->
            Element.column [] <|
                List.map fromInlineMarkdown <|
                    inlines


fromMarkdown : Float -> Dict String (Element msg) -> Block b i -> Element msg
fromMarkdown scale customs block =
    case block of
        BlankLine _ ->
            Element.none

        Heading _ level inlines ->
            case level of
                1 ->
                    title scale <| Inline.extractText inlines

                2 ->
                    section scale <| Inline.extractText inlines

                3 ->
                    subsection scale <| Inline.extractText inlines

                _ ->
                    inlines
                        |> List.map fromInlineMarkdown
                        |> Element.column []

        ThematicBreak ->
            Element.el [ Element.width Element.fill ] <| Element.none

        Paragraph _ inlines ->
            Element.paragraph [] <|
                List.map fromInlineMarkdown inlines

        CodeBlock _ codeStr ->
            case customs |> Dict.get (codeStr |> String.replace "\n" "") of
                Just element ->
                    element

                Nothing ->
                    Element.paragraph
                        [ Background.color <| black
                        , Font.family <| List.singleton Font.monospace
                        ]
                    <|
                        List.singleton <|
                            Element.text <|
                                codeStr

        BlockQuote blocks ->
            blocks
                |> List.map (fromMarkdown scale customs)
                |> Element.paragraph [ Font.italic ]

        List model items ->
            items
                |> List.map
                    (\item ->
                        [ Element.el
                            [ Element.width <| Element.px <| 50
                            , Element.height <| Element.fill
                            , Element.alignRight
                            , Element.alignTop
                            , Font.size 40
                            ]
                          <|
                            Element.text "•"
                        , item
                            |> List.map (fromMarkdown scale customs)
                            |> Element.paragraph [ ]
                        ]
                            |> Element.row [ Element.width <| Element.fill ]
                    )
                |> Element.column [ Element.spacing 10, Element.width <| Element.fill ]

        {- List.map
           (List.map Block.toHtml
               >> List.concat
               >> Html.li []
           )
           items
           |> (case model.type_ of
                   Ordered startInt ->
                       if startInt == 1 then
                           Html.ol []

                       else
                           Html.ol [ Attributes.start startInt ]

                   Unordered ->
                       Html.ul []
              )
           |> (\a -> (::) a [])
           |> List.map Element.html
           |> Element.column []
        -}
        PlainInlines inlines ->
            inlines
                |> List.map fromInlineMarkdown
                |> Element.column []

        Block.Custom customBlock blocks ->
            blocks
                |> List.map (fromMarkdown scale customs)
                |> Element.column []
