module GreenFields.Data.Building exposing (Building(..), generate, getDegree, getName, getSymbol)

import Array
import Random exposing (Generator)


type Building
    = Query


generate : Generator Building
generate =
    Random.constant Query


getName : Building -> String
getName building =
    case building of
        Query ->
            "Query"


getSymbol : Building -> String
getSymbol building =
    let
        array =
            building
                |> getName
                |> String.toList
                |> Array.fromList

        arr2 =
            array |> Array.filter (\char -> (char /= 'a') && (char /= 'e') && (char /= 'i') && (char /= 'o') && (char /= 'u'))
    in
    [ array |> Array.get 0
    , arr2 |> Array.get ((arr2 |> Array.length) // 2)
    , array |> Array.get (array |> Array.length |> (+) -1)
    ]
        |> List.filterMap identity
        |> String.fromList


getDegree : Building -> Int
getDegree building =
    case building of
        Query ->
            0
