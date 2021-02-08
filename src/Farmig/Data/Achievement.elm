module Farmig.Data.Achievement exposing (Achievement(..), get, next, smallerThen)

import Farmig.Data.Cell exposing (Cell(..))
import Farmig.Data.Food exposing (Food(..))
import Farmig.Data.Item exposing (Item(..))


type Achievement
    = StartedGame
    | AteACherry
    | MovedALog
    | PickedUpWater
    | WateredAPlant
    | AteABerry
    | PickedUpAxe
    | ChoppedDownATree
    | AteACarrot
    | PickedUpShit
    | AteAMelon
    | AteAnApple
    | ReachedDesert


toInt : Achievement -> Int
toInt achievement =
    case achievement of
        StartedGame ->
            0

        AteACherry ->
            1

        MovedALog ->
            2

        PickedUpWater ->
            3

        WateredAPlant ->
            4

        AteABerry ->
            5

        PickedUpAxe ->
            6

        ChoppedDownATree ->
            7

        AteACarrot ->
            8

        PickedUpShit ->
            9

        AteAMelon ->
            10

        AteAnApple ->
            11

        ReachedDesert ->
            12


fromInt : Int -> Maybe Achievement
fromInt int =
    case int of
        0 ->
            Just StartedGame

        1 ->
            Just AteACherry

        2 ->
            Just MovedALog

        3 ->
            Just PickedUpWater

        4 ->
            Just WateredAPlant

        5 ->
            Just AteABerry

        6 ->
            Just PickedUpAxe

        7 ->
            Just ChoppedDownATree

        8 ->
            Just AteACarrot

        9 ->
            Just PickedUpShit

        10 ->
            Just AteAMelon

        11 ->
            Just AteAnApple

        12 ->
            Just ReachedDesert

        _ ->
            Nothing


next : Achievement -> Maybe Achievement
next =
    toInt >> (+) 1 >> fromInt


smallerThen : Achievement -> Achievement -> Bool
smallerThen a1 a2 =
    toInt a1 > toInt a2


get : Achievement -> { icon : Cell, title : String, challenge : String }
get achievement =
    case achievement of
        StartedGame ->
            { icon = Ground
            , title = "You started the game!"
            , challenge = "But can you start the game?"
            }

        AteACherry ->
            { icon = Food Cherry
            , title = "You ate a cherry!"
            , challenge = "But can you also eat a cherry?"
            }

        PickedUpWater ->
            { icon = Item Water
            , title = "You picked up some water!"
            , challenge = "But can you also find some water?"
            }

        WateredAPlant ->
            { icon = Seed Berry
            , title = "You watered a plant!"
            , challenge = "But can you also water a plant?"
            }

        AteABerry ->
            { icon = Food Berry
            , title = "You ate a berry!"
            , challenge = "But can you also grow a berry bush?"
            }

        MovedALog ->
            { icon = Wood
            , title = "You found a log!"
            , challenge = "But can you also find a log?"
            }

        PickedUpAxe ->
            { icon = Item Axe
            , title = "You picked up an axe!"
            , challenge = "But can you also find a axe?"
            }

        ChoppedDownATree ->
            { icon = Item Axe
            , title = "You used your axe!"
            , challenge = "But can you also chop down a tree?"
            }

        AteACarrot ->
            { icon = Food Carrot
            , title = "You ate a carrot!"
            , challenge = "But can you also harvest a carrot?"
            }

        PickedUpShit ->
            { icon = Item Shit
            , title = "You picked up shit!"
            , challenge = "Did you know rabbits eat carrots aswell?"
            }

        AteAMelon ->
            { icon = Food Melon
            , title = "You ate a melon!"
            , challenge = "Did you know you can use shit as fertilizer?"
            }

        AteAnApple ->
            { icon = Food Apple
            , title = "You ate an apple!"
            , challenge = "But can you also grow an apple tree?"
            }

        ReachedDesert ->
            { icon = Food Meat
            , title = "You reached the desert!"
            , challenge = "But can you reach level 25?"
            }
