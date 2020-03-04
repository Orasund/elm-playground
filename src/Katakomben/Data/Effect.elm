module Katakomben.Data.Effect exposing (ConditionType(..), Effect(..), toString)

import Element exposing (Color)
import Framework.Color as Color
import Katakomben.Data.Card exposing (Card)
import Katakomben.Data.Monster exposing (Monster)


type ConditionType
    = HasMoney Int
    | HasHealth Int
    | HasMaxHealth Int
    | HasAttack Int
    | HasFullHealth


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
    | AddCard Card
    | SetAttack Int
    | AddAttack Int
    | AddHealth Int
    | AddMaxHealth Int
    | Attack
    | AddMoney Int
    | SetMaxHealth Int
    | Conditional ConditionType Effect


conditionToString : ConditionType -> String
conditionToString cond =
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


toString : Effect -> ( String, Maybe Color )
toString effect =
    case effect of
        Restart ->
            ( "Restart", Just Color.green )

        NextCard ->
            ( "Next Card", Nothing )

        AddRandomWeapon int ->
            ( "Add random Weapon", Nothing )

        AddRandomHealItem int ->
            ( "Add random healing item", Nothing )

        AddLoot int ->
            ( "Add random loot", Nothing )

        AddRandomUndead int ->
            ( "Add random undead monster", Just Color.red )

        AddRandomVermin int ->
            ( "Add random vermin monster", Just Color.red )

        RemoveCard ->
            ( "Remove this card", Nothing )

        SetAttack int ->
            ( String.fromInt int ++ " Attack ", Just Color.green )

        SetMaxHealth int ->
            ( String.fromInt (2 + int) ++ " Max Health", Just Color.green )

        AddAttack int ->
            ( (if int > 0 then
                "+"

               else
                ""
              )
                ++ String.fromInt int
                ++ " Attack "
            , if int > 0 then
                Just Color.green

              else
                Just Color.red
            )

        AddHealth int ->
            ( (if int > 0 then
                "+"

               else
                ""
              )
                ++ String.fromInt int
                ++ " Health"
            , if int > 0 then
                Just Color.green

              else
                Just Color.red
            )

        Attack ->
            ( "Damage the monster"
            , Nothing
            )

        AddMaxHealth int ->
            ( (if int > 0 then
                "+"

               else
                ""
              )
                ++ String.fromInt int
                ++ " Max Health"
            , if int > 0 then
                Just Color.green

              else
                Just Color.red
            )

        AddMoney int ->
            ( (if int > 0 then
                "+"

               else
                ""
              )
                ++ String.fromInt int
                ++ " Money"
            , if int > 0 then
                Just Color.green

              else
                Just Color.red
            )

        AddPreviousCard card ->
            ( "Add a card under the deck"
            , Nothing
            )

        AddCard card ->
            ( "Add a card on top of the deck"
            , Nothing
            )

        Conditional cond e ->
            let
                ( string, color ) =
                    e |> toString
            in
            ( "if " ++ (cond |> conditionToString) ++ ": " ++ string
            , color
            )
