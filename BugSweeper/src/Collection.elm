module Collection exposing (Collection, Variant(..), add, bugs, empty, get, insert, member, variantToString)

import BugSpecies exposing (BugSpecies)
import Dict exposing (Dict)


type Variant
    = Cute
    | Royal


type alias Collection =
    Dict
        String
        { bug : BugSpecies
        , variants : Dict String Variant
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


insert : BugSpecies -> Variant -> Collection -> Collection
insert bug variant =
    Dict.update (BugSpecies.toString bug)
        (\maybe ->
            maybe
                |> Maybe.map
                    (\entry ->
                        { entry | variants = Dict.insert (variantToString variant) variant entry.variants }
                    )
                |> Maybe.withDefault { bug = bug, variants = Dict.singleton (variantToString variant) variant }
                |> Just
        )


get : BugSpecies -> Collection -> List Variant
get bug collection =
    collection
        |> Dict.get (BugSpecies.toString bug)
        |> Maybe.map .variants
        |> Maybe.map Dict.values
        |> Maybe.withDefault []


member : BugSpecies -> Collection -> Bool
member bug =
    Dict.member (BugSpecies.toString bug)


add : Collection -> Collection -> Collection
add c1 c2 =
    c1
        |> Dict.foldl
            (\_ entry c ->
                entry.variants
                    |> Dict.foldl
                        (\_ variant ->
                            insert entry.bug variant
                        )
                        c
            )
            c2


bugs : Collection -> List BugSpecies
bugs collection =
    collection
        |> Dict.values
        |> List.map .bug
