module Level exposing (..)

import Array exposing (Array)
import Expression exposing (Expression(..), Operator(..), Symbol(..))


type alias Level =
    { inputs : List Symbol
    , goal : Expression
    , withVar : Bool
    }


errorLevel : Level
errorLevel =
    { inputs =
        [ NumberSymbol 1
        , NumberSymbol 2
        , NumberSymbol 3
        , OpSymbol PlusOp
        , OpSymbol TimesOp
        , VarSymbol
        ]
    , withVar = True
    , goal = Error
    }


levelSelect : Level
levelSelect =
    { inputs =
        [ NumberSymbol 0
        , NumberSymbol 1
        , NumberSymbol 2
        , NumberSymbol 3
        , NumberSymbol 4
        , OpSymbol PlusOp
        , OpSymbol TimesOp
        ]
    , withVar = False
    , goal = Number 0
    }


levels : Array Level
levels =
    errorLevel
        :: divideTrack
        ++ timesTrack
        ++ varTrack
        ++ finalTrack
        ++ [ { inputs =
                [ NumberSymbol 1
                , NumberSymbol 2
                , NumberSymbol 3
                , OpSymbol TimesOp
                ]
             , withVar = True
             , goal = Number 16
             }
           , { inputs =
                [ NumberSymbol 2
                , NumberSymbol 3
                , NumberSymbol 4
                , OpSymbol PlusOp
                , OpSymbol TimesOp
                ]
             , withVar = True
             , goal = Number 33
             }
           , setVarToOp
           , { inputs =
                [ NumberSymbol 1
                , OpSymbol PlusOp
                , OpSymbol TimesOp
                , OpSymbol DividedOp
                ]
             , withVar = True
             , goal = Number -999
             }
           , { inputs =
                [ NumberSymbol 2
                , NumberSymbol 3
                , NumberSymbol 4
                , OpSymbol DividedOp
                ]
             , withVar = False
             , goal = Number 14
             }
           , { inputs = []
             , withVar = True
             , goal = Number 6
             }
           ]
        |> Array.fromList


finalTrack : List Level
finalTrack =
    [ { inputs =
            [ NumberSymbol 2
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 32
      }
    , { inputs =
            [ NumberSymbol 2
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 88
      }
    , { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            , OpSymbol DividedOp
            ]
      , withVar = True
      , goal = DivisionByZero
      }
    ]


varTrack : List Level
varTrack =
    [ { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 46
      }
    , { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 66
      }
    ]


timesTrack : List Level
timesTrack =
    [ { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = False
      , goal = Number 12
      }
    , { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = False
      , goal = Number 1
      }
    ]


divideTrack : List Level
divideTrack =
    [ { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol DividedOp
            ]
      , withVar = False
      , goal = Number 1
      }
    , { inputs =
            [ NumberSymbol 2
            , NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol DividedOp
            ]
      , withVar = False
      , goal = Number 12
      }
    ]


plusTrack : List Level
plusTrack =
    [ { inputs =
            [ NumberSymbol 1
            , NumberSymbol 2
            , NumberSymbol 3
            , OpSymbol PlusOp
            ]
      , withVar = False
      , goal = Number 5
      }
    , { inputs =
            [ NumberSymbol 1
            , NumberSymbol 2
            , NumberSymbol 3
            , OpSymbol PlusOp
            ]
      , withVar = False
      , goal = Number 15
      }
    ]


setVarToOp : Level
setVarToOp =
    { inputs =
        [ NumberSymbol 1
        , NumberSymbol 2
        , NumberSymbol 3
        , OpSymbol TimesOp
        , VarSymbol
        ]
    , withVar = False
    , goal = Number 18
    }
