module HeroForge.Data.ConditionType exposing (ConditionType(..), toString)

import HeroForge.Data.Level as Level exposing (Level(..))
import HeroForge.Data.Item as Item exposing (Item)

type ConditionType
    = HasMoney Int
    | HasHealth Int
    | HasMaxHealth Int
    | HasAttack Int
    | HasFullHealth
    | Not ConditionType
    | And ConditionType ConditionType
    | Or ConditionType ConditionType
    | Success
    | HasBeatenLevel Level
    | HasItem Item


toString : ConditionType -> String
toString cond =
    case cond of
        HasMoney int ->
            "MONEY>=" ++ String.fromInt int

        HasHealth int ->
            "HEALTH>=" ++ String.fromInt int

        HasMaxHealth int ->
            "MAXHEALTH>=" ++ String.fromInt int

        HasAttack int ->
            "ATTACK>=" ++ String.fromInt int

        HasFullHealth ->
            "full health"

        Not c ->
            "not " ++ toString c

        And c1 c2 ->
            toString c1 ++ " and " ++ toString c2

        Or c1 c2 ->
            toString c1 ++ " or " ++ toString c2

        Success ->
            "true"

        HasBeatenLevel level ->
            "beaten " ++ Level.toString level

        HasItem item ->
            "owns " ++ Item.toString item
