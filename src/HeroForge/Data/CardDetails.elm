module HeroForge.Data.CardDetails exposing (CardDetails, getDetails, onFreed, onMailDelivery)

import HeroForge.Data.Card exposing (Card(..))
import HeroForge.Data.ConditionType exposing (ConditionType(..))
import HeroForge.Data.Effect exposing (Effect(..))
import HeroForge.Data.Item exposing (Item(..))
import HeroForge.Data.Level exposing (Level(..))
import HeroForge.Data.Loot exposing (LootSort(..))
import HeroForge.Data.Monster as Monster


type alias CardDetails =
    { desc : List String
    , left : Maybe ( String, List Effect )
    , right : ( String, List Effect )
    }


onMailDelivery : Level -> List Effect
onMailDelivery level =
    case level of
        Village ->
            []

        Forest ->
            [ AddCard <| Inn ]

        Mountains ->
            []


onFreed : Level -> List Effect
onFreed level =
    case level of
        Village ->
            []

        Forest ->
            [ AddCard <|
                Spawner Quest <|
                    Endboss
                        { monster = Monster.bossBandidLeader
                        , minion = Monster.bandid
                        }
            , ExitArea

            --, AddCard <|
            --    Camp
            , AddCard <|
                Entrance Mountains
            , AddCard <|
                Info "A new area has appeared."
            , AddItem (Mail Forest)
            , AddCard <|
                Info "You recieved a letter for the post office."
            ]

        Mountains ->
            []


getDetails : Card -> CardDetails
getDetails card =
    case card of
        Entrance level ->
            { desc = [ "Entrance" ]
            , left =
                Just
                    ( "Enter"
                    , [ NextCard
                      , SetCurrentLevel level
                      ]
                    )
            , right =
                ( "Continue"
                , [ NextCard
                  , Conditional (HasBeatenLevel level) ExitArea
                  , SetCurrentLevel level
                  ]
                )
            }

        Death ->
            { desc = []
            , left = Nothing
            , right = ( "Restart", [ RemoveCard, Restart ] )
            }

        Spawner item c ->
            { desc = []
            , left = Just ( "Continue", [ NextCard ] )
            , right =
                ( "Spawn"
                , [ NextCard
                  , Conditional (HasItem item) <|
                        AddCard c
                  , Conditional (HasItem item) <|
                        RemoveItem item
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
            , left = Just ( "Continue", [ NextCard ] )
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
                Just
                    ( "Continue"
                    , [ NextCard
                      , AddMoney -monster.attack
                      ]
                    )
            , right =
                ( "Attack"
                , [ Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) Attack)
                  , Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) (AddHealth -monster.attack))
                  , Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) (AddAttack -1))
                  , Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) (AddMaxHealth -1))
                  ]
                )
            }

        Endboss { monster, minion } ->
            { desc = [ "Attack: " ++ String.fromInt monster.attack ]
            , left =
                Just
                    ( "Continue"
                    , [ AddPreviousCard (Enemy minion)
                      , NextCard
                      , AddMoney -monster.attack
                      ]
                    )
            , right =
                ( "Attack"
                , [ Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) Attack)
                  , Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) (AddHealth -monster.attack))
                  , Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) (AddAttack -1))
                  , Conditional (HasAttack 1) (Conditional (HasHealth monster.attack) (AddMaxHealth -1))
                  ]
                )
            }

        Camp ->
            { desc = [ "+1 Health and", "1 Attack" ]
            , left = Nothing
            , right =
                ( "Restock"
                , [ NextCard
                  , Conditional (Not HasFullHealth) (AddHealth 1)
                  , Conditional (HasAttack 0) (SetAttack 1)
                  ]
                )
            }

        Shop cost item ->
            { desc = [ item.name ++ " for " ++ String.fromInt cost ++ " Money" ]
            , left =
                Just
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
            , left = Nothing
            , right =
                ( "Continue"
                , [ RemoveCard
                  ]
                )
            }

        PostOffice ->
            { desc = [ "Find mail to unlock new buildings in the village." ]
            , left = Nothing
            , right =
                ( "Continue"
                , [ NextCard
                  , DeliverMail
                  ]
                )
            }

        Inn ->
            { desc = [ "Quest: Kill the leader of the bandits" ]
            , left =
                Just
                    ( "Reject"
                    , [ NextCard
                      ]
                    )
            , right =
                ( "Accept"
                , [ NextCard
                  , AddItem Quest
                  ]
                )
            }
