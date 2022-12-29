module Data.Improvement exposing (..)


type Improvement
    = GetOneGoldEachLevel
    | MinecartCanCollect
    | TrainCanCollect
    | GameRunsFaster


asList : List Improvement
asList =
    [ GetOneGoldEachLevel
    , MinecartCanCollect
    , TrainCanCollect
    , GameRunsFaster
    ]


toString : Improvement -> String
toString improvement =
    case improvement of
        GetOneGoldEachLevel ->
            "Get one gold each level"

        MinecartCanCollect ->
            "Minecart can collect items"

        TrainCanCollect ->
            "Train can collect items"

        GameRunsFaster ->
            "Game runs faster"
