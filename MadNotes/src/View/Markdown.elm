module View.Markdown exposing (view)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


view : String -> Html msg
view string =
    string
        |> Markdown.Parser.parse
        |> Result.mapError (\list -> list |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        |> (\result ->
                case result of
                    Ok list ->
                        list

                    Err errs ->
                        [ Html.text errs ]
           )
        |> Html.div []


renderer : Markdown.Renderer.Renderer (Html msg)
renderer =
    { html = Markdown.Html.oneOf []
    , heading =
        \{ level, children } ->
            let
                tag =
                    case level of
                        Block.H1 ->
                            h1 [ class "elm-book-serif" ]

                        Block.H2 ->
                            h2 [ class "elm-book-serif" ]

                        Block.H3 ->
                            h3 [ class "elm-book-serif" ]

                        Block.H4 ->
                            h4 [ class "elm-book-sans" ]

                        Block.H5 ->
                            h5 [ class "elm-book-sans" ]

                        Block.H6 ->
                            h6 [ class "elm-book-sans" ]
            in
            div [ class "elm-book-md" ] [ tag children ]
    , paragraph =
        \children ->
            div [ class "elm-book-md elm-book-serif elm-book-md__default" ] [ p [] children ]
    , hardLineBreak = div [ class "elm-book-md" ] [ br [] [] ]
    , blockQuote =
        \children ->
            div [ class "elm-book-md elm-book-serif" ]
                [ blockquote [] children
                ]
    , strong = strong []
    , emphasis = Html.em []
    , strikethrough = span []
    , codeSpan =
        \children ->
            span [ class "elm-book-md" ] [ code [ class "elm-book-monospace" ] [ text children ] ]
    , link =
        \link children ->
            a
                [ href link.destination
                , title (Maybe.withDefault "" link.title)
                ]
                children
    , image =
        \image ->
            div [ class "elm-book-md" ]
                [ img [ src image.src, alt image.alt, title <| Maybe.withDefault "" image.title ] []
                ]
    , text = text
    , unorderedList =
        \items ->
            div [ class "elm-book-md elm-book-serif elm-book-md__default" ]
                [ ul []
                    (items
                        |> List.map
                            (\item ->
                                case item of
                                    Block.ListItem task children ->
                                        let
                                            checkbox =
                                                case task of
                                                    Block.NoTask ->
                                                        text ""

                                                    Block.IncompleteTask ->
                                                        input
                                                            [ Attr.disabled True
                                                            , Attr.checked False
                                                            , type_ "checkbox"
                                                            ]
                                                            []

                                                    Block.CompletedTask ->
                                                        input
                                                            [ Attr.disabled True
                                                            , Attr.checked True
                                                            , type_ "checkbox"
                                                            ]
                                                            []
                                        in
                                        li [] (checkbox :: children)
                            )
                    )
                ]
    , orderedList =
        \startingIndex items ->
            div [ class "elm-book-md elm-book-serif elm-book-md__default" ]
                [ ol [ Attr.start startingIndex ]
                    (List.map (li []) items)
                ]
    , codeBlock =
        \{ body } ->
            div [ class "elm-book-md" ]
                [ Html.pre [ class "elm-book-md__code-default elm-book-monospace elm-book-shadows-light" ] [ text body ]
                ]
    , thematicBreak =
        div [ class "elm-book-md" ] [ hr [] [] ]
    , table =
        \children ->
            div [ class "elm-book-md elm-book-md__default elm-book-sans" ] [ Html.table [] children ]
    , tableHeader = thead []
    , tableBody = tbody []
    , tableRow = tr []
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
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            th attrs
    , tableCell = \_ children_ -> td [] children_
    }
