module Ecocards.Data.Animal exposing
    ( Animal
    , Biome(..)
    , bear
    , biomeFromString
    , biomeToString
    , cat
    , fish
    , mouse
    , otter
    , rabbit
    , wolf
    )

import Set exposing (Set)


type Biome
    = Plain
    | River
    | Forest


biomeToString : Biome -> String
biomeToString biome =
    case biome of
        Plain ->
            "plain"

        River ->
            "river"

        Forest ->
            "forest"


biomeFromString : String -> Maybe Biome
biomeFromString string =
    case string of
        "plain" ->
            Just Plain

        "river" ->
            Just River

        "forest" ->
            Just Forest

        _ ->
            Nothing


type alias Animal =
    { symbol : String
    , strength : Int
    , biome : Biome
    , eats : Set String
    }


fish : Animal
fish =
    { symbol = "🐟"
    , strength = 1
    , biome = River
    , eats = Set.empty
    }


mouse : Animal
mouse =
    { symbol = "🐁"
    , strength = 1
    , biome = Plain
    , eats = Set.empty
    }


otter : Animal
otter =
    { symbol = "\u{1F9A6}"
    , strength = 2
    , biome = River
    , eats =
        [ River ]
            |> List.map biomeToString
            |> Set.fromList
    }


cat : Animal
cat =
    { symbol = "🐈"
    , strength = 2
    , biome = Plain
    , eats =
        [ Plain, River ]
            |> List.map biomeToString
            |> Set.fromList
    }


wolf : Animal
wolf =
    { symbol = "🐺"
    , strength = 3
    , biome = Forest
    , eats =
        [ Forest, Plain ]
            |> List.map biomeToString
            |> Set.fromList
    }


rabbit : Animal
rabbit =
    { symbol = "🐇"
    , strength = 1
    , biome = Forest
    , eats = Set.empty
    }


bear : Animal
bear =
    { symbol = "🐻"
    , strength = 4
    , biome = Forest
    , eats =
        [ Forest, Plain, River ]
            |> List.map biomeToString
            |> Set.fromList
    }
