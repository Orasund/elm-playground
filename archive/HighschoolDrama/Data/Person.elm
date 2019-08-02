module HighschoolDrama.Data.Person exposing (Person, chooseName, npc, player)

import HighschoolDrama.Data exposing (Orientation(..), Sex(..))
import Random exposing (Generator)


type alias Person =
    { name : String
    , sex : Maybe Sex
    , orientation : Orientation
    }


maleName : List String
maleName =
    [ "👦"
    , "👦\u{1F3FB}"
    , "👦\u{1F3FC}"
    , "👦\u{1F3FD}"
    , "👦\u{1F3FE}"
    , "👦\u{1F3FF}"
    , "👨"
    , "👨\u{1F3FB}"
    , "👨\u{1F3FC}"
    , "👨\u{1F3FD}"
    , "👨\u{1F3FE}"
    , "👨\u{1F3FF}"
    , "👨\u{200D}\u{1F9B1}"
    , "👨\u{1F3FB}\u{200D}\u{1F9B1}"
    , "👨\u{1F3FC}\u{200D}\u{1F9B1}"
    , "👨\u{1F3FD}\u{200D}\u{1F9B1}"
    , "👨\u{1F3FE}\u{200D}\u{1F9B1}"
    , "👨\u{1F3FF}\u{200D}\u{1F9B1}"
    , "👨\u{200D}\u{1F9B0}"
    , "👨\u{1F3FB}\u{200D}\u{1F9B0}"
    , "👨\u{1F3FC}\u{200D}\u{1F9B0}"
    , "👨\u{1F3FD}\u{200D}\u{1F9B0}"
    , "👨\u{1F3FE}\u{200D}\u{1F9B0}"
    , "👨\u{1F3FF}\u{200D}\u{1F9B0}"
    , "👱"
    , "👱\u{1F3FB}"
    , "👱\u{1F3FC}"
    , "👱\u{1F3FD}"
    , "👱\u{1F3FE}"
    , "👱\u{1F3FF}"
    , "👨\u{200D}\u{1F9B3}"
    , "👨\u{1F3FB}\u{200D}\u{1F9B3}"
    , "👨\u{1F3FC}\u{200D}\u{1F9B3}"
    , "👨\u{1F3FD}\u{200D}\u{1F9B3}"
    , "👨\u{1F3FE}\u{200D}\u{1F9B3}"
    , "👨\u{1F3FF}\u{200D}\u{1F9B3}"
    , "\u{1F9D4}"
    , "\u{1F9D4}\u{1F3FB}"
    , "\u{1F9D4}\u{1F3FC}"
    , "\u{1F9D4}\u{1F3FD}"
    , "\u{1F9D4}\u{1F3FE}"
    , "\u{1F9D4}\u{1F3FF}"
    ]


femaleName : List String
femaleName =
    [ "👧"
    , "👧\u{1F3FB}"
    , "👧\u{1F3FC}"
    , "👧\u{1F3FD}"
    , "👧\u{1F3FE}"
    , "👧\u{1F3FF}"
    , "👩"
    , "👩\u{1F3FB}"
    , "👩\u{1F3FC}"
    , "👩\u{1F3FD}"
    , "👩\u{1F3FE}"
    , "👩\u{1F3FF}"
    , "👩\u{200D}\u{1F9B1}"
    , "👩\u{1F3FB}\u{200D}\u{1F9B1}"
    , "👩\u{1F3FC}\u{200D}\u{1F9B1}"
    , "👩\u{1F3FD}\u{200D}\u{1F9B1}"
    , "👩\u{1F3FE}\u{200D}\u{1F9B1}"
    , "👩\u{1F3FF}\u{200D}\u{1F9B1}"
    , "👩\u{200D}\u{1F9B0}"
    , "👩\u{1F3FB}\u{200D}\u{1F9B0}"
    , "👩\u{1F3FC}\u{200D}\u{1F9B0}"
    , "👩\u{1F3FD}\u{200D}\u{1F9B0}"
    , "👩\u{1F3FE}\u{200D}\u{1F9B0}"
    , "👩\u{1F3FF}\u{200D}\u{1F9B0}"
    , "👱\u{200D}♀"
    , "👱\u{1F3FB}\u{200D}♀"
    , "👱\u{1F3FC}\u{200D}♀"
    , "👱\u{1F3FD}\u{200D}♀"
    , "👱\u{1F3FE}\u{200D}♀"
    , "👱\u{1F3FF}\u{200D}♀"
    , "👩\u{200D}\u{1F9B3}"
    , "👩\u{1F3FB}\u{200D}\u{1F9B3}"
    , "👩\u{1F3FC}\u{200D}\u{1F9B3}"
    , "👩\u{1F3FD}\u{200D}\u{1F9B3}"
    , "👩\u{1F3FE}\u{200D}\u{1F9B3}"
    , "👩\u{1F3FF}\u{200D}\u{1F9B3}"
    ]


neutralName : List String
neutralName =
    [ "\u{1F9D2}"
    , "\u{1F9D2}\u{1F3FB}"
    , "\u{1F9D2}\u{1F3FC}"
    , "\u{1F9D2}\u{1F3FD}"
    , "\u{1F9D2}\u{1F3FE}"
    , "\u{1F9D2}\u{1F3FF}"
    , "\u{1F9D1}"
    , "\u{1F9D1}\u{1F3FB}"
    , "\u{1F9D1}\u{1F3FC}"
    , "\u{1F9D1}\u{1F3FD}"
    , "\u{1F9D1}\u{1F3FE}"
    , "\u{1F9D1}\u{1F3FF}"
    , "👨\u{1F3FB}\u{200D}\u{1F9B2}"
    , "👨\u{1F3FC}\u{200D}\u{1F9B2}"
    , "👨\u{1F3FD}\u{200D}\u{1F9B2}"
    , "👨\u{1F3FE}\u{200D}\u{1F9B2}"
    , "👨\u{1F3FF}\u{200D}\u{1F9B2}"
    ]


chooseName : Maybe Sex -> Generator String
chooseName maybeSex =
    Random.uniform "👨\u{200D}\u{1F9B2}" <|
        List.concat
            [ neutralName
            , case maybeSex of
                Just Male ->
                    maleName

                Just Female ->
                    femaleName

                Nothing ->
                    neutralName
            ]


npc : { includeSex : Bool } -> Generator Person
npc _ =
    Debug.todo "write npc generator"


player : Maybe Sex -> Person
player _ =
    Debug.todo "write person constructor"
