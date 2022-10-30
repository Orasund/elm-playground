module View.Chapter exposing (..)

import Data.Page exposing (Page)
import Data.Shared exposing (State)
import Dict
import ElmBook exposing (Msg)
import ElmBook.Chapter
import ElmBook.Custom
import Html exposing (Html)
import Layout
import List.Extra
import Random exposing (Generator)
import View.Markdown
import View.Quiz


type alias Chapter =
    ElmBook.Chapter.Chapter State


view : String -> List Page -> ElmBook.Custom.Chapter State (Html (Msg State))
view title content =
    ElmBook.Chapter.chapter title
        |> ElmBook.Chapter.withChapterInit
            (\state ->
                ( state
                , Cmd.none
                )
            )
        |> ElmBook.Chapter.renderStatefulComponent
            (\state ->
                let
                    chapter =
                        state.chapters
                            |> Dict.get title
                            |> Maybe.withDefault
                                (Data.Shared.chapterState
                                    { pages = [], closingText = "", previousTitle = Nothing }
                                )

                    readPreviousChapter =
                        chapter.previousTitle
                            |> Maybe.andThen (\t -> state.chapters |> Dict.get t)
                            |> Maybe.map (\c -> c.currentPage == c.maxPage || c.isClosed)
                            |> Maybe.withDefault True
                in
                if not readPreviousChapter then
                    """> Du fühlst dich noch nicht bereit dieses Kapitel aufzuschlagen."""
                        |> View.Markdown.view

                else if chapter.isClosed then
                    chapter.closingText |> View.Markdown.view

                else
                    [ content
                        |> List.take (chapter.currentPage + 1)
                        |> List.indexedMap (\i page -> "## Seite " ++ (i + 1 |> String.fromInt) ++ "\n" ++ page.content)
                        |> String.join "\n\n"
                        |> View.Markdown.view
                    , if chapter.currentPage < chapter.maxPage then
                        content
                            |> List.Extra.getAt chapter.currentPage
                            |> Maybe.andThen .quiz
                            |> Maybe.withDefault
                                { question = "Du spürst das dringende Verlangen auf die nächste Seite zu blättern."
                                , answers = [ "Pause einlegen", "Umblättern" ]
                                , correctAnswer = 2
                                }
                            |> View.Quiz.view title

                      else
                        Layout.none
                    ]
                        |> Layout.column []
            )


mapLines : (String -> Generator String) -> String -> Generator String
mapLines fun string =
    string
        |> String.split "\n\n"
        |> List.foldr
            (\line ->
                Random.andThen
                    (\list ->
                        fun line
                            |> Random.map (\l -> l :: list)
                    )
            )
            (Random.constant [])
        |> Random.map (String.join "\n\n")


redact : String -> String
redact =
    String.map
        (\c ->
            if
                Char.isAlphaNum c
                    || List.member c [ 'ä', 'ö', 'ü', 'Ä', 'Ö', 'Ü', 'ß' ]
            then
                '█'

            else
                c
        )


redactAtRandom : Float -> String -> Generator String
redactAtRandom dangerRate line =
    Random.float 0 (logBase e dangerRate)
        |> Random.map
            (\float ->
                if float > 0.5 then
                    line
                        |> redact

                else
                    line
            )


type alias RenderOptions =
    Float
