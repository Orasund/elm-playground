module Katakomben.Data.Game exposing (Direction(..), Game, Msg(..), current, init, update)

import Katakomben.Data.Card exposing (Card(..), Level(..))
import Katakomben.Data.CardDetails as CardDetails
import Katakomben.Data.Deck as Deck exposing (Deck)
import Katakomben.Data.Effect exposing (Effect(..))
import Katakomben.Data.Item as Item
import Katakomben.Data.Monster as Monster
import Katakomben.Data.Name as Name
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
                        |> Deck.jumpTo (Entrence CatacombsOfDunkelhall)

                Nothing ->
                    [ Entrence CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Tomb CatacombsOfDunkelhall
                    , Shrine CatacombsOfDunkelhall
                    , Entrence GraveyardChapel
                    , Shrine GraveyardChapel
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
            init (Just game.deck)|> Random.constant

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
                            | deck = game.deck |> Deck.addNext (Loot item)
                        }
                    )

        AddRandomHealItem quality ->
            Item.generateHealing quality
                |> Random.map
                    (\item ->
                        { game
                            | deck = game.deck |> Deck.addNext (Loot item)
                        }
                    )

        AddLoot quality ->
            Item.generateLoot quality
                |> Random.map
                    (\item ->
                        { game
                            | deck = game.deck |> Deck.addNext (Loot item)
                        }
                    )

        AddRandomUndead quality ->
            Monster.generateUndead quality
                |> Random.map
                    (\monster ->
                        { game
                            | deck = game.deck |> Deck.addNext (Enemy monster)
                        }
                    )

        AddRandomVermin quality ->
            Monster.generateVermin quality
                |> Random.map
                    (\monster ->
                        { game
                            | deck = game.deck |> Deck.addNext (Enemy monster)
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

        AddHealth amount ->
            { game
                | health =
                    game.health
                        + amount
                        |> clamp 0 game.maxHealth
                , deck =
                    game.deck
                        |> (if game.health + amount <= 0 then
                                Deck.addNext Death >> Deck.next

                            else
                                identity
                           )
            }
                |> Random.constant

        SetAttack amount ->
            { game
                | attack = amount
            }
                |> Random.constant

        SetMaxHealth amount ->
            { game
                | maxHealth = amount + 2
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

        PayForHeal ->
            let
                amount =
                    game.maxHealth - game.health
            in
            Random.constant <|
                if game.money >= amount then
                    { game
                        | money = game.money - amount
                        , health = game.maxHealth
                    }

                else
                    { game
                        | health = game.health + game.money
                        , maxHealth = game.health + game.money
                        , money = 0
                    }

        Attack ->
            case game |> current of
                Enemy monster ->
                    { game
                        | deck =
                            game.deck
                                |> (if monster.health - game.attack > 0 then
                                        Deck.addNext
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
                                |> Deck.remove
                    }
                        |> Random.constant

                _ ->
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
