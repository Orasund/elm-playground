module Level exposing (..)

import Array exposing (Array)
import Expression exposing (Expression(..), Operator(..), Symbol(..))


type alias Level =
    { inputs : List Symbol
    , goal : Int
    }


errorLevel : Level
errorLevel =
    { inputs =
        [ NumberSymbol 1
        , NumberSymbol 2
        , OpSymbol PlusOp
        , OpSymbol TimesOp
        , VarSymbol
        ]
    , goal = 0
    }


levels : Array Level
levels =
    []
        |> Array.fromList
