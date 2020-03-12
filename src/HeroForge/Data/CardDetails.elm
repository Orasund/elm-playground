module HeroForge.Data.CardDetails exposing (CardDetails, getDetails, onFreed)

import HeroForge.Data.Card exposing (Card(..), Level(..))
import HeroForge.Data.ConditionType exposing (ConditionType(..))
import HeroForge.Data.Effect exposing (Effect(..))
import HeroForge.Data.Item exposing (ItemSort(..))


type alias CardDetails =
    { desc : List String
    , left : ( String, List Effect )
    , right : ( String, List Effect )
    }


onFreed : Level -> List Effect
onFreed level =
    case level of
        Village ->
            []

        Forest ->
            [ ExitArea
            , AddCard <|
                Camp
            , AddCard <|
                Entrance Mountains
            , AddCard <|
                Info "A new area has appeared."
            ]

        Mountains ->
            []


getDetails : Card -> CardDetails
getDetails card =
    case card of
        Entrance level ->
            { desc = [ "Entrance" ]
            , left =
                ( "Continue"
                , [ NextCard
                  , SetCurrentLevel level
                  ]
                )
            , right = ( "Continue", [ NextCard, SetCurrentLevel level ] )
            }

        Death ->
            { desc = []
            , left = ( "", [] )
            , right = ( "Restart", [ RemoveCard, Restart ] )
            }

        Spawner cond c ->
            { desc = []
            , left = ( "Continue", [ NextCard ] )
            , right =
                ( "Spawn"
                , [ NextCard
                  , Conditional cond <|
                        AddCard c
                  ]
                )
            }

        Loot item ->
            { desc =
                [ item.name
                , case item.sort of
                    Weapon amount ->
                        "Attack " ++ String.fromInt amount

                    Healing amount ->
                        "Health +" ++ String.fromInt amount

                    Value amount ->
                        String.fromInt amount ++ " Money"

                    Armor amount ->
                        "Max Health " ++ String.fromInt (amount + 2)
                ]
            , left = ( "Continue", [ NextCard ] )
            , right =
                case item.sort of
                    Weapon amount ->
                        ( "Equip"
                        , [ RemoveCard
                          , Conditional (HasAttack amount) <|
                                AddCard (Loot item)
                          , SetAttack amount
                          ]
                        )

                    Healing amount ->
                        ( "Use"
                        , [ RemoveCard
                          , Conditional HasFullHealth <|
                                AddCard (Loot item)
                          , AddHealth amount
                          ]
                        )

                    Value amount ->
                        ( "Sell"
                        , [ RemoveCard
                          , AddMoney amount
                          ]
                        )

                    Armor amount ->
                        ( "Equip"
                        , [ RemoveCard
                          , Conditional (HasMaxHealth (amount + 2)) <|
                                AddCard (Loot item)
                          , SetMaxHealth amount
                          ]
                        )
            }

        Enemy monster ->
            { desc =
                [ monster.name
                , "Attack: " ++ String.fromInt monster.attack
                ]
            , left =
                ( "Continue"
                , [ NextCard
                  , AddHealth -monster.attack
                  ]
                )
            , right =
                ( "Attack"
                , [ Attack
                  , AddHealth -monster.attack
                  , AddAttack -1
                  , AddMaxHealth -1
                  ]
                )
            }

        Endboss { monster, minion } ->
            { desc = [ "Attack: " ++ String.fromInt monster.attack ]
            , left =
                ( "Continue"
                , [ AddPreviousCard (Enemy minion)
                  , NextCard
                  , AddHealth -monster.attack
                  ]
                )
            , right =
                ( "Attack"
                , [ Attack
                  , AddHealth -monster.attack
                  , AddAttack -1
                  , AddMaxHealth -1
                  ]
                )
            }

        Camp ->
            { desc = [ "+1 Health or", "+1 Attack" ]
            , left =
                ( "Rest"
                , [ NextCard
                  , AddHealth 1
                  ]
                )
            , right =
                ( "Equip basic weapon"
                , [ NextCard
                  , SetAttack 1
                  ]
                )
            }

        Shop cost item ->
            { desc = [ item.name ++ " for " ++ String.fromInt cost ++ " Money" ]
            , left =
                ( "Continue"
                , [ NextCard
                  ]
                )
            , right =
                ( "Buy"
                , [ Conditional (HasMoney cost) (AddCard (Loot item))
                  , Conditional (HasMoney cost) (AddMoney -cost)
                  ]
                )
            }

        Info string ->
            { desc = [ string ]
            , left =
                ( "Continue"
                , [ RemoveCard
                  ]
                )
            , right =
                ( "Continue"
                , [ RemoveCard
                  ]
                )
            }
