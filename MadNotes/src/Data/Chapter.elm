module Data.Chapter exposing (..)

import Data.Page exposing (Page)


type alias Chapter =
    { title : String
    , pages : List Page
    , closingText : String
    }


new : { title : String, pages : List Page, closingText : String } -> Chapter
new args =
    { title = args.title
    , pages = args.pages
    , closingText = args.closingText
    }
