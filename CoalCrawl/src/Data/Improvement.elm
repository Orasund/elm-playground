module Data.Improvement exposing (..)


type Improvement
    = GetOneGoldEachLevel
    | MinecartCanCollect


asList : List Improvement
asList =
    [ GetOneGoldEachLevel
    , MinecartCanCollect
    ]


toString : Improvement -> String
toString improvement =
    case improvement of
        GetOneGoldEachLevel ->
            "Get one gold each level"

        MinecartCanCollect ->
            "Minecart can collect items"
