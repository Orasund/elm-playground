module View.Quiz exposing (..)

import Data.Page exposing (Quiz)
import Data.Shared exposing (State)
import ElmBook exposing (Msg)
import Html exposing (Html)
import Html.Attributes as Attr
import Layout
import View.Button
import View.Markdown


view : String -> Quiz -> Html (Msg State)
view chapter args =
    [ Html.hr [] []
    , Html.br [] []
    , [ View.Markdown.view args.question
      , Html.br [] []
      , args.answers
            |> List.indexedMap
                (\i answer ->
                    View.Button.view
                        { label = answer
                        , onPress =
                            if i + 1 == args.correctAnswer then
                                Data.Shared.nextPage chapter

                            else
                                Data.Shared.closeChapter chapter
                        }
                )
            |> Layout.row [ Layout.spacing 8 ]
            |> Layout.el [ Attr.class "elm-book-md elm-book-serif elm-book-md__default" ]
      ]
        |> Html.blockquote []
    ]
        |> Html.div
            [ Attr.class "elm-book-md elm-book-serif"
            , Attr.style "margin" "0"
            ]
