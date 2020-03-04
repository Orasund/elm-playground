module Katakomben.Data.Game exposing (Direction(..), Game, Msg(..), current, init, update)

import Katakomben.Data.Card exposing (Card(..), Level(..))
import Katakomben.Data.CardDetails as CardDetails
import Katakomben.Data.Deck as Deck exposing (Deck)
import Katakomben.Data.Effect exposing (ConditionType(..), Effect(..))
import Katakomben.Data.Item as Item exposing (ItemSort(..))
import Katakomben.Data.Monster as Monster
import Random exposing (Generator)


type Direction
    = Left
    | Right


type alias Game =
    { deck : Deck Card
    , health : Int
    , maxHealth : Int
    , money : Int
    , attack : Int
    }


current : Game -> Card
current =
    .deck
        >> Deck.current
        >> Maybe.withDefault Death


type Msg
    = Chosen Direction


init : Maybe (Deck Card) -> Game
init maybeDeck =
    let
        deck =
            case maybeDeck of
                Just d ->
                    d
                        |> Deck.jumpTo (Entrance CatacombsOfDunkelhall)

                Nothing ->
                    [ Entrance CatacombsOfDunkelhall
                    , Camp
                    , Tomb CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Shrine CatacombsOfDunkelhall
                    , Entrance Village
                    , Shop 10
                        { name = "Old Lether Armor"
                        , sort = Armor 2
                        , desc = ""
                        }
                    , Shrine Village
                    ]
                        |> Deck.fromList
    in
    { deck = deck
    , maxHealth = 2
    , health = 2
    , money = 0
    , attack = 0
    }


applyEffect : Effect -> Game -> Generator Game
applyEffect effect game =
    case effect of
        Restart ->
            init (Just game.deck) |> Random.constant

        NextCard ->
            { game
                | deck =
                    game.deck
                        |> Deck.next
            }
                |> Random.constant

        AddRandomWeapon quality ->
            Item.generateWeapon quality
                |> Random.map
                    (\item ->
                        { game
                            | deck = game.deck |> Deck.add (Loot item)
                        }
                    )

        AddRandomHealItem quality ->
            Item.generateHealing quality
                |> Random.map
                    (\item ->
                        { game
                            | deck = game.deck |> Deck.add (Loot item)
                        }
                    )

        AddLoot quality ->
            Item.generateLoot quality
                |> Random.map
                    (\item ->
                        { game
                            | deck = game.deck |> Deck.add (Loot item)
                        }
                    )

        AddRandomUndead quality ->
            Monster.generateUndead quality
                |> Random.map
                    (\monster ->
                        { game
                            | deck = game.deck |> Deck.add (Enemy monster)
                        }
                    )

        AddRandomVermin quality ->
            Monster.generateVermin quality
                |> Random.map
                    (\monster ->
                        { game
                            | deck = game.deck |> Deck.add (Enemy monster)
                        }
                    )

        RemoveCard ->
            { game
                | deck =
                    game.deck
                        |> Deck.remove
            }
                |> Random.constant

        AddPreviousCard card ->
            { game
                | deck =
                    game.deck
                        |> Deck.addPrevious card
            }
                |> Random.constant

        AddCard card ->
            { game
                | deck =
                    game.deck
                        |> Deck.add card
            }
                |> Random.constant

        AddHealth amount ->
            { game
                | health =
                    game.health
                        + amount
                        |> clamp 0 game.maxHealth
                , deck =
                    game.deck
                        |> (if game.health + amount <= 0 then
                                Deck.add Death

                            else if (game.health == game.maxHealth) && (amount > 0) then
                                Deck.add <| Info "You are on full health. The healing effect did nothing."

                            else
                                identity
                           )
            }
                |> Random.constant

        SetAttack amount ->
            { game
                | attack = max amount game.attack
                , deck =
                    game.deck
                        |> (if game.attack > amount then
                                Deck.add <| Info "You have a better Weapon equipped."

                            else
                                identity
                           )
            }
                |> Random.constant

        SetMaxHealth amount ->
            { game
                | maxHealth = max (amount + 2) game.maxHealth
            }
                |> Random.constant

        AddMaxHealth amount ->
            { game
                | maxHealth = max 2 (game.maxHealth + amount)
            }
                |> Random.constant

        AddAttack amount ->
            { game
                | attack = game.attack + amount |> max 0
            }
                |> Random.constant

        AddMoney amount ->
            { game
                | money =
                    game.money
                        + amount
                        |> max 0
            }
                |> Random.constant

        Attack ->
            case game |> current of
                Enemy monster ->
                    { game
                        | deck =
                            game.deck
                                |> Deck.remove
                                |> (if monster.health - game.attack > 0 then
                                        Deck.add
                                            (Enemy
                                                { monster
                                                    | health =
                                                        monster.health
                                                            - game.attack
                                                            |> clamp 0 monster.health
                                                    , attack =
                                                        monster.attack
                                                            - 1
                                                            |> clamp 1 monster.attack
                                                }
                                            )

                                    else
                                        identity
                                   )
                    }
                        |> Random.constant

                _ ->
                    game |> Random.constant

        Conditional cond e ->
            if
                case cond of
                    HasHealth amount ->
                        game.health >= amount

                    HasMoney amount ->
                        game.money >= amount

                    HasMaxHealth amount ->
                        game.maxHealth >= amount

                    HasAttack amount ->
                        game.attack >= amount

                    HasFullHealth ->
                        game.health >= game.maxHealth
            then
                applyEffect e game

            else
                game |> Random.constant


update : Msg -> Game -> Generator Game
update msg game =
    case msg of
        Chosen dir ->
            game
                |> current
                |> CardDetails.getDetails
                |> (case dir of
                        Left ->
                            .left

                        Right ->
                            .right
                   )
                |> Tuple.second
                |> List.foldl (\effect -> Random.andThen (applyEffect effect))
                    (Random.constant game)
