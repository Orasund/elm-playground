module HeroForge.Data.Monster exposing (Monster, bandid, bossBandidLeader, bossPaterErhard, fragileSkeleton, generateUndead, generateVermin)

import Random exposing (Generator)


type alias Monster =
    { name : String
    , attack : Int
    , health : Int
    , desc : String
    , loot : Int
    }


bandid : Monster
bandid =
    { name = "Bandid"
    , attack = 1
    , health = 1
    , desc = ""
    , loot = 3
    }


bossBandidLeader : Monster
bossBandidLeader =
    { name = "Bandid Leader"
    , health = 4
    , attack = 1
    , desc = ""
    , loot = 5
    }


bossPaterErhard : Monster
bossPaterErhard =
    { name = "Undead Pater Erhard"
    , desc = ""
    , health = 4
    , attack = 1
    , loot = 3
    }


fragileSkeleton : Monster
fragileSkeleton =
    { name = "Fragile Skeleton"
    , desc = ""
    , health = 1
    , attack = 1
    , loot = 3
    }


generateVermin : Int -> Generator Monster
generateVermin quality =
    case quality of
        _ ->
            Random.weighted
                ( 1
                , { name = "White Rat"
                  , desc = "A big fat old rat. Big scars tell the story of it fights."
                  , health = 4
                  , attack = 3
                  , loot = 3
                  }
                )
                [ ( 3
                  , { name = "Big Rat"
                    , desc = "A big rat. Kill it."
                    , health = 3
                    , attack = 1
                    , loot = 3
                    }
                  )
                ]


generateUndead : Int -> Generator Monster
generateUndead quality =
    case quality of
        0 ->
            Random.weighted
                ( 1
                , { name = "Rotten Zombie-Corpse"
                  , desc = "A old zombies, whos flesh is mostly rotten away. I smells really, really bad."
                  , health = 2
                  , attack = 1
                  , loot = 3
                  }
                )
                [ ( 4
                  , { name = "Fragile Skeleton"
                    , desc = "A slow an clumsy skeleton. It wobbles about like a drunk. You've seen tuffer enemies."
                    , health = 1
                    , attack = 1
                    , loot = 3
                    }
                  )
                ]

        _ ->
            Random.weighted
                ( 1
                , { name = "Skeleton"
                  , desc = "An old Skeleton. When you think it's dead, it continues living."
                  , health = 3
                  , attack = 2
                  , loot = 3
                  }
                )
                [ ( 3
                  , { name = "Zombie-Corpse"
                    , desc = "A walking dead. Who has brought it to life? No one knows."
                    , health = 4
                    , attack = 1
                    , loot = 3
                    }
                  )
                ]
