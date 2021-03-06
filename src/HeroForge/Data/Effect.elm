module HeroForge.Data.Effect exposing (Effect(..), toString)

import Color as C exposing (Color)
import Element
import HeroForge.Data.Card as Card exposing (Card)
import HeroForge.Data.ConditionType as ConditionType exposing (ConditionType)
import HeroForge.Data.Item as Item exposing (Item)
import HeroForge.Data.Level as Level exposing (Level)
import HeroForge.View.Color as Color


type Effect
    = Restart
    | NextCard
    | RemoveCard
    | AddPreviousCard Card
    | AddCard Card
    | AddAttack Int
    | AddHealth Int
    | AddMaxHealth Int
    | SetAttack Int
    | SetMaxHealth Int
    | SetCurrentLevel Level
    | Attack
    | AddMoney Int
    | Conditional ConditionType Effect
    | ExitArea
    | RestartArea
    | AddItem Item
    | RemoveItem Item
    | DeliverMail


toString : Effect -> ( String, Maybe Color )
toString effect =
    case effect of
        Restart ->
            ( "Restart", Just Color.green )

        NextCard ->
            ( "Next Card", Just Color.cyan )

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
            ( "Add " ++ (card |> Card.toString |> .name) ++ " under the deck"
            , Nothing
            )

        AddCard card ->
            ( "Add " ++ (card |> Card.toString |> .name) ++ " on top of the deck"
            , Nothing
            )

        SetCurrentLevel level ->
            ( "Entering " ++ (level |> Level.toString)
            , Nothing
            )

        Conditional cond e ->
            let
                ( string, color ) =
                    e |> toString
            in
            ( "if " ++ (cond |> ConditionType.toString) ++ ": " ++ string
            , color
            )

        ExitArea ->
            ( "Exit the area"
            , Just Color.cyan
            )

        RestartArea ->
            ( "Restart the area"
            , Just Color.cyan
            )

        AddItem item ->
            ( "Adds " ++ Item.toString item ++ " to inventory"
            , Just Color.green
            )

        RemoveItem item ->
            ( "Removes " ++ Item.toString item ++ " from inventory"
            , Nothing
            )

        DeliverMail ->
            ( "Deliver mail"
            , Nothing
            )
