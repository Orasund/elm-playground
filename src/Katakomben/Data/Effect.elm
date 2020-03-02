module Katakomben.Data.Effect exposing (Effect(..), toString)

import Katakomben.Data.Card exposing (Card)
import Katakomben.Data.Monster exposing (Monster)


type Effect
    = Restart
    | NextCard
    | AddRandomWeapon Int
    | AddRandomHealItem Int
    | AddLoot Int
    | AddRandomUndead Int
    | AddRandomVermin Int
    | RemoveCard
    | AddPreviousCard Card
    | SetAttack Int
    | AddAttack Int
    | AddHealth Int
    | Attack
    | AddMoney Int
    | PayForHeal
    | SetMaxHealth Int


toString : Effect -> String
toString effect =
    case effect of
        Restart ->
            "Restart"

        NextCard ->
            "Next Card"

        AddRandomWeapon int ->
            "Add random Weapon"

        AddRandomHealItem int ->
            "Add random healing item"

        AddLoot int ->
            "Add random loot"

        AddRandomUndead int ->
            "Add random undead monster"

        AddRandomVermin int ->
            "Add random vermin monster"

        RemoveCard ->
            "Remove this card"

        SetAttack int ->
            String.fromInt int ++ " Attack "

        SetMaxHealth int ->
            String.fromInt (2 + int) ++ " Max Health"

        AddAttack int ->
            (if int > 0 then
                "+"

             else
                ""
            )
                ++ String.fromInt int
                ++ " Attack "

        AddHealth int ->
            (if int > 0 then
                "+"

             else
                ""
            )
                ++ String.fromInt int
                ++ " Health"

        Attack ->
            "Damage the monster"

        AddMoney int ->
            (if int > 0 then
                "+"

             else
                ""
            )
                ++ String.fromInt int
                ++ " Money"

        PayForHeal ->
            "Pay for full heal"

        AddPreviousCard card ->
            "Add a card under the stack"
