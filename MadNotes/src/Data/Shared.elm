module Data.Shared exposing (..)

import Data.Chapter exposing (Chapter)
import Dict exposing (Dict)
import Random exposing (Seed)


type alias State =
    { seed : Maybe Seed
    , madness : Int
    , offset : Float
    , chapters : Dict String ChapterState
    }


type alias ChapterState =
    { currentPage : Int
    , maxPage : Int
    , isClosed : Bool
    , closingText : String
    , previousTitle : Maybe String
    }


chapterState : { pages : List String, closingText : String, previousTitle : Maybe String } -> ChapterState
chapterState args =
    { currentPage = 0
    , maxPage = List.length args.pages - 1
    , isClosed = False
    , closingText = args.closingText
    , previousTitle = args.previousTitle
    }


init : List Chapter -> State
init chapters =
    { seed = Nothing
    , madness = 0
    , offset = 0
    , chapters =
        List.map2
            (\previousTitle chapter ->
                ( chapter.title
                , chapterState
                    { pages = chapter.pages |> List.map (\page -> page.content)
                    , closingText = chapter.closingText
                    , previousTitle = previousTitle
                    }
                )
            )
            (Nothing :: (chapters |> List.map (\{ title } -> Just title)))
            chapters
            |> Dict.fromList
    }


setOffset : Int -> State -> State
setOffset offset state =
    { state | offset = state.offset + toFloat (state.madness * state.madness * modBy 10 offset) * 0.01 }


initialSeed : Seed -> State -> State
initialSeed seed state =
    if state.seed == Nothing then
        newSeed seed state

    else
        state


closeChapter : String -> State -> State
closeChapter title state =
    { state
        | chapters =
            state.chapters
                |> Dict.update title
                    (Maybe.map (\chapter -> { chapter | isClosed = True }))
    }


nextPage : String -> State -> State
nextPage title state =
    { state
        | chapters =
            state.chapters
                |> Dict.update title
                    (Maybe.map (\chapter -> { chapter | currentPage = chapter.currentPage + 1 }))
    }


newSeed : Seed -> State -> State
newSeed seed state =
    { state | seed = Just seed }
