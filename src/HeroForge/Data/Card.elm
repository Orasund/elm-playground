module HeroForge.Data.Card exposing (Card(..), Level(..), levelToString, toString)

import Element exposing (Attribute)
import Framework.Color as Color
import HeroForge.Data.ConditionType exposing (ConditionType)
import HeroForge.Data.Item exposing (Item, ItemSort(..))
import HeroForge.Data.Monster exposing (Monster)


type Level
    = Village
    | Forest
    | Mountains


levelToString : Level -> String
levelToString level =
    case level of
        Village ->
            "Village"

        Forest ->
            "Forest"

        Mountains ->
            "Mountains"


type Card
    = Entrance Level
    | Death
    | Loot Item
    | Enemy Monster
    | Endboss { monster : Monster, minion : Monster }
    | Camp
    | Spawner ConditionType Card
    | Shop Int Item
    | Info String


toString : Card -> { name : String, color : List (Attribute Never) }
toString card =
    case card of
        Entrance level ->
            { name = level |> levelToString
            , color = Color.warning
            }

        Death ->
            { name = "Death"
            , color = Color.dark
            }

        Spawner _ _ ->
            { name = "Spawner"
            , color = Color.simple
            }

        Loot item ->
            { name =
                case item.sort of
                    Weapon amount ->
                        "Weapon Item " ++ String.fromInt amount

                    Healing amount ->
                        "Healing Item " ++ String.fromInt amount

                    Value amount ->
                        "Value Item " ++ String.fromInt amount

                    Armor amount ->
                        "Armor Item " ++ String.fromInt (amount + 2)
            , color = Color.primary
            }

        Enemy monster ->
            { name =
                "Monster("
                    ++ String.fromInt monster.health
                    ++ ")"
            , color = Color.danger
            }

        Endboss { monster } ->
            { name =
                "Endboss "
                    ++ monster.name
                    ++ " (Health: "
                    ++ String.fromInt monster.health
                    ++ ")"
            , color = Color.danger
            }

        Camp ->
            { name = "Camp"
            , color = Color.light
            }

        Shop _ _ ->
            { name = "Shop"
            , color = Color.light
            }

        Info _ ->
            { name = "Info"
            , color = Color.info
            }
