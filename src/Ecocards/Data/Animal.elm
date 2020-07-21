module Ecocards.Data.Animal exposing (Animal, Biome, bear, cat, deer, fish, mouse, otter, wolf)


type Biome
    = Plain
    | River


type Behaviour
    = Predator Biome ( Int, Int )
    | Herbivores Int
    | Omnivorous ( Int, Int )


type alias Animal =
    { symbol : String
    , strength : Int
    , biome : Biome
    , behaviour : Behaviour
    }


fish : Animal
fish =
    { symbol = "🐟"
    , strength = 1
    , biome = River
    , behaviour = Herbivores 1
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
    , biome = Plain
    , behaviour = Predator River ( 1, 2 )
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
    , behaviour = Omnivorous ( 3, 6 )
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
