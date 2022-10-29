module Data.Page exposing (..)


type alias Page =
    { content : String
    , quiz : Maybe Quiz
    }


type alias Quiz =
    { question : String
    , answers : List String
    , correctAnswer : Int
    }


fromString : String -> Page
fromString content =
    { content = content
    , quiz = Nothing
    }


withQuiz :
    { question : String
    , answers : List String
    , correctAnswer : Int
    }
    -> Page
    -> Page
withQuiz question page =
    { page | quiz = Just question }
