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
        , OpSymbol TimesOp
        ]
    , withVar = False
    , goal = Number 0
    }


levels : Array Level
levels =
    errorLevel
        :: [ { inputs = []
             , withVar = True
             , goal = Number 6
             }
           , { inputs =
                [ NumberSymbol 2
                , NumberSymbol 3
                , NumberSymbol 4
                , OpSymbol TimesOp
                , OpSymbol DividedOp
                ]
             , withVar = True
             , goal = Number 999
             }
           ]
        ++ baseGame
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
                , OpSymbol TimesOp
                ]
             , withVar = True
             , goal = Number 33
             }
           , setVarToOp
           , { inputs =
                [ NumberSymbol 1
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


baseGame : List Level
baseGame =
    divideTrack
        ++ timesTrack
        ++ varTrack
        ++ [ divideByZero ]


divideByZero : Level
divideByZero =
    { inputs =
        [ NumberSymbol 2
        , NumberSymbol 3
        , NumberSymbol 4
        , OpSymbol TimesOp
        , OpSymbol DividedOp
        ]
    , withVar = True
    , goal = DivisionByZero
    }


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
    , { inputs =
            [ NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = True
      , goal = Number 256
      }
    , { inputs =
            [ NumberSymbol 4
            ]
      , withVar = True
      , goal = Number 40
      }
    ]


timesTrack : List Level
timesTrack =
    [ { inputs =
            [ NumberSymbol 3
            , NumberSymbol 4
            , OpSymbol TimesOp
            ]
      , withVar = False
      , goal = Number 12
      }
    , { inputs =
            [ NumberSymbol 3
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
            [ NumberSymbol 3
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
