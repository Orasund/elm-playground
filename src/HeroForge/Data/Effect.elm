module HeroForge.Data.Effect exposing (Effect(..), toString)

import Element exposing (Color)
import Framework.Color as Color
import HeroForge.Data.Card as Card exposing (Card, Level)
import HeroForge.Data.ConditionType as ConditionType exposing (ConditionType)


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

toString : Effect -> ( String, Maybe Color )
toString effect =
    case effect of
        Restart ->
            ( "Restart", Just Color.green )

        NextCard ->
            ( "Next Card", Nothing )

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
            ( "Entering " ++ (level |> Card.levelToString)
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
            ( "Exit the Area"
            , Nothing
            )
