module HeroForge.Data.Card exposing (Card(..), toString)

import Color as C exposing (Color)
import Element exposing (Attribute)
import HeroForge.Data.ConditionType exposing (ConditionType)
import HeroForge.Data.Item as Item exposing (Item)
import HeroForge.Data.Level as Level exposing (Level(..))
import HeroForge.Data.Loot exposing (Loot, LootSort(..))
import HeroForge.Data.Monster exposing (Monster)
import HeroForge.View.Color as Color


type Card
    = Entrance Level
    | Death
    | Loot Loot
    | Enemy Monster
    | Endboss { monster : Monster, minion : Monster }
    | Camp
    | Spawner Item Card
    | Shop Int Loot
    | Info String
    | PostOffice
    | Inn


toString : Card -> { name : String, symbol : String, color : Color }
toString card =
    case card of
        Entrance level ->
            { name = level |> Level.toString
            , symbol = "🚪"
            , color = Color.cyan
            }

        Death ->
            { name = "Death"
            , symbol = "💀"
            , color = Color.black
            }

        Spawner _ _ ->
            { name = "Spawner"
            , symbol = "🎇"
            , color = Color.white
            }

        Loot item ->
            { name =
                case item.sort of
                    Weapon amount ->
                        "Weapon Loot " ++ String.fromInt amount

                    Healing amount ->
                        "Healing Loot " ++ String.fromInt amount

                    Value amount ->
                        "Value Loot " ++ String.fromInt amount

                    Armor amount ->
                        "Armor Loot " ++ String.fromInt (amount + 2)
            , symbol =
                case item.sort of
                    Weapon _ ->
                        "🗡️"

                    Healing _ ->
                        "❤️"

                    Value _ ->
                        "💰"

                    Armor _ ->
                        "🛡️"
            , color = Color.green
            }

        Enemy monster ->
            { name =
                "Monster("
                    ++ String.fromInt monster.health
                    ++ ")"
            , symbol = "👾"
            , color = Color.red
            }

        Endboss { monster } ->
            { name =
                "Endboss "
                    ++ monster.name
                    ++ " (Health: "
                    ++ String.fromInt monster.health
                    ++ ")"
            , symbol = "👾"
            , color = Color.red
            }

        Camp ->
            { name = "Camp"
            , symbol = "⛺"
            , color = Color.white
            }

        Shop _ _ ->
            { name = "Shop"
            , symbol = "\u{1F6D2}"
            , color = Color.white
            }

        Info _ ->
            { name = "Info"
            , symbol = "❗"
            , color = Color.cyan
            }

        PostOffice ->
            { name = "Post Office"
            , symbol = "📯"
            , color = Color.white
            }

        Inn ->
            { name = "Inn"
            , symbol = "🛎"
            , color = Color.white
            }
