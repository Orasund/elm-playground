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
        Entrence name ->
            { name =
                "Entrence to "
                    ++ (case name of
                            CatacombsOfDunkelhall ->
                                "the catacombs of Dunkelhall"

                            GraveyardChapel ->
                                "the graveyard chapel"
                       )
            , desc =
                case name of
                    CatacombsOfDunkelhall ->
                        "The Catacombs of the lords and ladys of Dunkelhall: Many have entered, but none have returned."

                    GraveyardChapel ->
                        "The chapel above the catacombs is sparly visited and has definitely seen better days."
            , left = ( "Continue", [ NextCard ] )
            , right = ( "Continue", [ NextCard ] )
            , color = Color.light
            }

        Death ->
            { name = "Death"
            , desc = "You are Dead"
            , left = ( "", [] )
            , right = ( "Restart", [ RemoveCard, Restart ] )
            , color = Color.dark
            }

        Tomb level ->
            { name = "Tomb"
            , desc = "Rest in Peace. It might be best to not desturb the dead."
            , left = ( "Continue", [ NextCard ] )
            , right =
                ( "Open"
                , case level of
                    CatacombsOfDunkelhall ->
                        [ AddLoot 0
                        , AddLoot 0
                        , AddRandomHealItem 0
                        , AddRandomUndead 0
                        , RemoveCard
                        ]

                    GraveyardChapel ->
                        [ AddRandomVermin 0
                        , AddLoot 0
                        , RemoveCard
                        ]
                )
            , color = Color.simple
            }

        Loot item ->
            { name = item.name
            , desc = item.desc
            , left = ( "Continue", [ NextCard ] )
            , right =
                case item.sort of
                    Weapon amount ->
                        ( "Equip"
                        , [ SetAttack amount
                          , RemoveCard
                          ]
                        )

                    Healing amount ->
                        ( "Use"
                        , [ AddHealth amount
                          , RemoveCard
                          ]
                        )

                    Value amount ->
                        ( "Sell"
                        , [ AddMoney amount
                          , RemoveCard
                          ]
                        )

                    Armor amount ->
                        ( "Equip"
                        , [ SetMaxHealth amount
                          , RemoveCard
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
            , desc = monster.desc
            , left =
                ( "Continue"
                , [ NextCard
                  , AddHealth -monster.attack
                  ]
                )
            , right =
                ( "Attack"
                , [ AddHealth -monster.attack
                  , Attack
                  , AddAttack -1
                  ]
                )
            , color = Color.danger
            }

        Camp ->
            { name = "Camp"
            , desc = ""
            , left =
                ( "Rest"
                , [ AddHealth 1
                  , NextCard
                  ]
                )
            , right =
                ( "Equip basic weapon"
                , [ SetAttack 1
                  , NextCard
                  ]
                )
            , color = Color.success
            }

        Shrine level ->
            case level of
                GraveyardChapel ->
                    { name = "Altar"
                    , desc = "The Chapel demands a donation. Heal yourself for 1 Health/Money."
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
                    , color = Color.success
                    }

                CatacombsOfDunkelhall ->
                    { name = "Tomb of Pater Erhard"
                    , desc = "The old tomb sits at the end of the hall. It's the only Tomb that looks like it might contain something of value."
                    , left =
                        ( "Continue"
                        , [ AddPreviousCard
                                (Tomb CatacombsOfDunkelhall)
                          , NextCard
                          ]
                        )
                    , right =
                        ( "Open"
                        , [ AddPreviousCard
                                (Tomb CatacombsOfDunkelhall)
                          , AddLoot 1
                          , AddLoot 1
                          , AddLoot 1
                          , AddRandomUndead 1
                          , NextCard
                          ]
                        )
                    , color = Color.warning
                    }
