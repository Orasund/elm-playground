module Ecocards.Data.Animal exposing
    ( Animal
    , Behaviour(..)
    , Biome
    , bear
    , behaviourDescription
    , behaviourToString
    , biomeToString
    , cat
    , deer
    , fish
    , getAmounts
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
            "🌄"

        River ->
            "🌊"


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
    , behaviour : Behaviour
    }


getAmounts : Animal -> ( Int, Int )
getAmounts animal =
    case animal.behaviour of
        Predator amounts ->
            amounts

        Herbivores _ ->
            ( 0, 0 )

        Omnivorous amounts ->
            amounts


fish : Animal
fish =
    { symbol = "🐟"
    , strength = 1
    , biome = River
    , behaviour = Herbivores 0
    }


mouse : Animal
mouse =
    { symbol = "🐁"
    , strength = 1
    , biome = Plain
    , behaviour = Herbivores 1
    }


otter : Animal
otter =
    { symbol = "\u{1F9A6}"
    , strength = 2
    , biome = River
    , behaviour = Predator ( 1, 2 )
    }


cat : Animal
cat =
    { symbol = "🐈"
    , strength = 2
    , biome = Plain
    , behaviour = Omnivorous ( 2, 4 )
    }


wolf : Animal
wolf =
    { symbol = "🐺"
    , strength = 3
    , biome = Plain
    , behaviour = Predator ( 3, 6 )
    }


deer : Animal
deer =
    { symbol = "\u{1F98C}"
    , strength = 2
    , biome = Plain
    , behaviour = Herbivores 2
    }


bear : Animal
bear =
    { symbol = "🐻"
    , strength = 4
    , biome = Plain
    , behaviour = Omnivorous ( 4, 8 )
    }
