module Ecocards.Data.Animal exposing
    ( Animal
    , Behaviour(..)
    , Biome(..)
    , bear
    , biomeToString
    , cat
    , deer
    , fish
    , mouse
    , otter
    , wolf
    )


type Biome
    = Plain
    | River


biomeToString : Biome -> String
biomeToString biome =
    case biome of
        Plain ->
            "plain"

        River ->
            "river"


type Behaviour
    = Predator ( Int, Int )
    | Herbivores Int
    | Omnivorous ( Int, Int )


behaviourToString : Behaviour -> String
behaviourToString behaviour =
    case behaviour of
        Predator ( min, max ) ->
            "Pred." ++ String.fromInt min ++ "-" ++ String.fromInt max

        Herbivores int ->
            "Herb." ++ String.fromInt int

        Omnivorous ( min, max ) ->
            "Omni." ++ String.fromInt min ++ "-" ++ String.fromInt max


behaviourDescription : Behaviour -> { title : String, desc : String }
behaviourDescription behaviour =
    case behaviour of
        Predator ( min, max ) ->
            { title = "Predator " ++ String.fromInt min ++ " - " ++ String.fromInt max
            , desc =
                "Remove animals such that the total strength lyes within "
                    ++ String.fromInt min
                    ++ " and "
                    ++ String.fromInt max
                    ++ ". Removed animals need to be weaker and of the same Biome. "
                    ++ "If the total strength of all removed animals is "
                    ++ String.fromInt max
                    ++ ", add a copy of your card to the top of your deck. Tap your card."
            }

        Herbivores int ->
            { title = "Herbivores " ++ String.fromInt int
            , desc =
                "Tap your card if " ++ String.fromInt int ++ " or more cards have been tapped."
            }

        Omnivorous ( min, max ) ->
            { title = "Omnivorous " ++ String.fromInt min ++ " - " ++ String.fromInt max
            , desc =
                "Remove animals such that the total strength lyes within "
                    ++ String.fromInt min
                    ++ " and "
                    ++ String.fromInt max
                    ++ ". Removed animals need to be weaker. "
                    ++ "If the total strength of all removed animals is "
                    ++ String.fromInt max
                    ++ ", add a copy of your card to the top of your deck. Tap your card."
            }


type alias Animal =
    { symbol : String
    , strength : Int
    , biome : Biome
    , eats : List Biome
    }


fish : Animal
fish =
    { symbol = "🐟"
    , strength = 1
    , biome = River
    , eats = []
    }


mouse : Animal
mouse =
    { symbol = "🐁"
    , strength = 1
    , biome = Plain
    , eats = []
    }


otter : Animal
otter =
    { symbol = "\u{1F9A6}"
    , strength = 2
    , biome = River
    , eats = [ River ]
    }


cat : Animal
cat =
    { symbol = "🐈"
    , strength = 2
    , biome = Plain
    , eats = [ Plain, River ]
    }


wolf : Animal
wolf =
    { symbol = "🐺"
    , strength = 3
    , biome = Plain
    , eats = [ Plain ]
    }


deer : Animal
deer =
    { symbol = "\u{1F98C}"
    , strength = 2
    , biome = Plain
    , eats = []
    }


bear : Animal
bear =
    { symbol = "🐻"
    , strength = 4
    , biome = Plain
    , eats = [ Plain, River ]
    }
