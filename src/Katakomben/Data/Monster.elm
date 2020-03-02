module Katakomben.Data.Monster exposing (Monster, generateUndead, generateVermin)

import Random exposing (Generator)


type alias Monster =
    { name : String
    , attack : Int
    , health : Int
    , desc : String
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
                  }
                )
                [ ( 3
                  , { name = "Big Rat"
                    , desc = "A big rat. Kill it."
                    , health = 3
                    , attack = 1
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
                  }
                )
                [ ( 4
                  , { name = "Fragile Skeleton"
                    , desc = "A slow an clumsy skeleton. It wobbles about like a drunk. You've seen tuffer enemies."
                    , health = 1
                    , attack = 1
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
                  }
                )
                [ ( 3
                  , { name = "Zombie-Corpse"
                    , desc = "A walking dead. Who has brought it to life? No one knows."
                    , health = 4
                    , attack = 1
                    }
                  )
                ]
