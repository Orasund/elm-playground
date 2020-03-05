module Katakomben.Data.CardDetails exposing (CardDetails, getDetails)

import Element exposing (Attribute, Color)
import Framework.Color as Color
import Katakomben.Data.Card exposing (Card(..), Level(..))
import Katakomben.Data.Effect exposing (ConditionType(..), Effect(..))
import Katakomben.Data.Item exposing (Item, ItemSort(..))
import Katakomben.Data.Monster exposing (Monster)


type alias CardDetails =
    { name : String
    , desc : String
    , left : ( String, List Effect )
    , right : ( String, List Effect )
    , color : List (Attribute Never)
    }


getDetails : Card -> CardDetails
getDetails card =
    case card of
        Entrance name ->
            { name =
                case name of
                    CatacombsOfDunkelhall ->
                        "Catacombs of Dunkelhall"

                    Village ->
                        "Village"

                    Forest ->
                        "Forest"
            , desc = "Entrance"
            , left = ( "Continue", [ NextCard ] )
            , right = ( "Continue", [ NextCard ] )
            , color = Color.warning
            }

        Death ->
            { name = "Death"
            , desc = ""
            , left = ( "", [] )
            , right = ( "Restart", [ RemoveCard, Restart ] )
            , color = Color.dark
            }

        Tomb ->
            { name = "Tomb"
            , desc = ""
            , left = ( "Continue", [ NextCard ] )
            , right =
                ( "Open"
                , [ RemoveCard
                  , AddLoot 0
                  , AddLoot 0
                  , AddRandomHealItem 0
                  , AddRandomUndead 0
                  ]
                )
            , color = Color.simple
            }

        Loot item ->
            { name = item.name
            , desc =
                case item.sort of
                    Weapon amount ->
                        "Attack " ++ String.fromInt amount

                    Healing amount ->
                        "Health +" ++ String.fromInt amount

                    Value amount ->
                        String.fromInt amount ++ " Money"

                    Armor amount ->
                        "Max Health " ++ String.fromInt (amount + 2)
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
            , color = Color.primary
            }

        Enemy monster ->
            { name =
                monster.name
                    ++ " (Health: "
                    ++ String.fromInt monster.health
                    ++ ")"
            , desc = "Attack: " ++ String.fromInt monster.attack
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
            , color = Color.danger
            }

        Camp ->
            { name = "Camp"
            , desc = "+1 Health or +1 Attack"
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
            , color = Color.light
            }

        Shrine level ->
            case level of
                Village ->
                    { name = "Tavern"
                    , desc = "Sleep for 1 Health/Money"
                    , left =
                        ( "Continue"
                        , [ NextCard
                          ]
                        )
                    , right =
                        ( "Pay for heal"
                        , [ Conditional (HasMoney 1) (AddHealth 1)
                          , Conditional (HasMoney 1) (AddMoney -1)
                          ]
                        )
                    , color = Color.light
                    }

                Forest ->
                    { name = "Bandit Hideout"
                    , desc = "Pay 1 Money or fight"
                    , left =
                        ( "Spawn Bandit"
                        , [ AddPreviousCard
                                (Enemy
                                    { name = "Bandit"
                                    , attack = 1
                                    , health = 2
                                    , desc = ""
                                    }
                                )
                          , NextCard
                          ]
                        )
                    , right =
                        ( "Pay"
                        , [ NextCard
                          , AddMoney -1
                          ]
                        )
                    , color = Color.light
                    }

                CatacombsOfDunkelhall ->
                    { name = "Tomb of Pater Erhard"
                    , desc = ""
                    , left =
                        ( "Continue"
                        , [ AddPreviousCard Tomb
                          , NextCard
                          ]
                        )
                    , right =
                        ( "Open"
                        , [ AddPreviousCard (Tomb)
                          , RemoveCard
                          , AddLoot 1
                          , AddLoot 1
                          , AddLoot 1
                          , AddRandomUndead 1
                          ]
                        )
                    , color = Color.light
                    }

        Shop cost item ->
            { name = "Shop "
            , desc = item.name ++ " for " ++ String.fromInt cost ++ " Money"
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
            , color = Color.light
            }

        Info string ->
            { name = string
            , desc = ""
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
            , color = Color.info
            }
