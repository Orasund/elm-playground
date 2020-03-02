module Katakomben.Data.Item exposing (Item, ItemSort(..), generateArmor, generateHealing, generateLoot, generateValue, generateWeapon)

import Random exposing (Generator)


type ItemSort
    = Weapon Int
    | Healing Int
    | Armor Int
    | Value Int


type alias Item =
    { name : String
    , sort : ItemSort
    , desc : String
    }


generateLoot : Int -> Generator Item
generateLoot quality =
    Random.weighted ( 1, generateArmor quality )
        [ ( 1, generateWeapon quality )
        , ( 1, generateHealing quality )
        , ( 3, generateValue quality )
        ]
        |> Random.andThen identity


generateArmor : Int -> Generator Item
generateArmor quality =
    case quality of
        0 ->
            Random.weighted
                ( 2
                , { name = "Old Lether Armor"
                  , sort = Armor 2
                  , desc = "An Old lether armor. It does not look fancy, but it does the job."
                  }
                )
                [ ( 1
                  , { name = "Wodden Buckler Shield"
                    , sort = Armor 1
                    , desc = "A small wodden shield. Better than nothing."
                    }
                  )
                ]

        _ ->
            Random.weighted
                ( 1
                , { name = "Shield"
                  , sort = Armor 3
                  , desc = "A big and proper shield. How nice."
                  }
                )
                [ ( 2
                  , { name = "Old Lether Armor"
                    , sort = Armor 2
                    , desc = "An Old lether armor. It does not look fancy, but it does the job."
                    }
                  )
                ]


generateWeapon : Int -> Generator Item
generateWeapon quality =
    case quality of
        0 ->
            Random.weighted
                ( 1
                , { name = "Rusty Knife"
                  , sort = Weapon 2
                  , desc = "A rusty Knife with the emblem of the Dunkelhall family on it."
                  }
                )
                [ ( 2
                  , { name = "Sharp Bone"
                    , sort = Weapon 1
                    , desc = "A sharp bone. You can't quite tell which part of the body this belongs to."
                    }
                  )
                ]

        _ ->
            Random.weighted
                ( 1
                , { name = "Rusty Short Sword"
                  , sort = Weapon 3
                  , desc = "An old short sword. It even has some ingravings."
                  }
                )
                [ ( 3
                  , { name = "Rusty Dagger"
                    , sort = Weapon 2
                    , desc = "A rusty little dager, this might come handy"
                    }
                  )
                ]


generateHealing : Int -> Generator Item
generateHealing quality =
    case quality of
        _ ->
            Random.weighted
                ( 1
                , { name = "Improvised Bandage"
                  , desc = "A piece of cloth, usefull to stop bleedings"
                  , sort = Healing 3
                  }
                )
                [ ( 3
                  , { name = "Used Bandages"
                    , sort = Healing 1
                    , desc = "Old Bandages, partly covered in blood. Not your blood."
                    }
                  )
                ]


generateValue : Int -> Generator Item
generateValue quality =
    case quality of
        _ ->
            Random.weighted
                ( 1
                , { name = "Broken Watch"
                  , desc = "A broken watch with some gold decorations"
                  , sort = Value 5
                  }
                )
                [ ( 4
                  , { name = "Silver Coin"
                    , desc = "A single silver coin"
                    , sort = Value 2
                    }
                  )
                , ( 10
                  , { name = "Pouch of Buttons"
                    , desc = "A collection of buttons in various colors. They have no use other then being sold."
                    , sort = Value 1
                    }
                  )
                ]
