module Fish.Name exposing (..)

import Random exposing (Generator)


random : Generator String
random =
    Random.map3
        (\town attribute feature ->
            town ++ " " ++ attribute ++ feature
        )
        towns
        attributes
        features


features : Generator String
features =
    Random.uniform "fish"
        [ "fin"
        , "back"
        , "jaw"
        , "flipper"
        , "tail"
        ]


attributes : Generator String
attributes =
    Random.uniform "Sun"
        [ "Fast"
        , "Slow"
        , "Shimmer"
        , "Dull"
        , "Moon"
        , "Winter"
        , "Spring"
        , "Summer"
        , "Autumn"
        ]


towns : Generator String
towns =
    Random.uniform "Vienna"
        [ "London"
        , "New York"
        , "Berlin"
        , "Paris"
        , "Hongkong"
        , "Tokyo"
        , "Peking"
        ]
