module Collection exposing (Collection, Variant(..), add, bugs, count, empty, get, insert, member, variantToString)

import Bug exposing (Bug)
import Dict exposing (Dict)


type Variant
    = Cute
    | Royal


type alias Collection =
    Dict
        String
        { bug : Bug
        , variants : Dict String Variant
        , caught : Int
        }


variantToString : Variant -> String
variantToString variant =
    case variant of
        Cute ->
            "Cute"

        Royal ->
            "Royal"


empty : Collection
empty =
    Dict.empty


insert : Int -> Bug -> Variant -> Collection -> Collection
insert n bug variant =
    Dict.update (Bug.toString bug)
        (\maybe ->
            maybe
                |> Maybe.map
                    (\entry ->
                        { entry
                            | variants = Dict.insert (variantToString variant) variant entry.variants
                            , caught = entry.caught + n
                        }
                    )
                |> Maybe.withDefault
                    { bug = bug
                    , variants = Dict.singleton (variantToString variant) variant
                    , caught = n
                    }
                |> Just
        )


get : Bug -> Collection -> List Variant
get bug collection =
    collection
        |> Dict.get (Bug.toString bug)
        |> Maybe.map .variants
        |> Maybe.map Dict.values
        |> Maybe.withDefault []


member : Bug -> Collection -> Bool
member bug =
    Dict.member (Bug.toString bug)


add : Collection -> Collection -> Collection
add c1 c2 =
    c1
        |> Dict.foldl
            (\_ entry c ->
                entry.variants
                    |> Dict.foldl
                        (\_ variant ->
                            insert entry.caught entry.bug variant
                        )
                        c
            )
            c2


bugs : Collection -> List Bug
bugs collection =
    collection
        |> Dict.values
        |> List.map .bug


count : Bug -> Collection -> Int
count bug collection =
    Dict.get (Bug.toString bug) collection
        |> Maybe.map .caught
        |> Maybe.withDefault 0
