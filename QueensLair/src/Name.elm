module Name exposing (..)

import Random exposing (Generator)


generate : Generator String
generate =
    Random.map2 (++)
        femaleFirstSyllable
        femaleSecondSyllable


femaleFirstSyllable : Generator String
femaleFirstSyllable =
    Random.uniform "Ade"
        [ "Ame"
        , "Bea"
        , "Brun"
        , "Cleo"
        , "Ema"
        , "Eri"
        , "El"
        , "Mar"
        , "Mira"
        , "Sig"
        ]


femaleSecondSyllable : Generator String
femaleSecondSyllable =
    Random.uniform "laide"
        [ "lia"
        , "triz"
        , "hilde"
        , "lette"
        , "goria"
        , "linda"
        , "tilda"
        , "tine"
        , "bel"
        , "rid"
        ]
