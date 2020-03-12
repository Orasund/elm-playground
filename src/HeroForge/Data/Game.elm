module HeroForge.Data.Game exposing (Direction(..), Game, Msg(..), current, init, update)

import HeroForge.Data.Card as Card exposing (Card(..), Level(..))
import HeroForge.Data.CardDetails as CardDetails
import HeroForge.Data.ConditionType exposing (ConditionType(..))
import HeroForge.Data.Deck as Deck exposing (Deck)
import HeroForge.Data.Effect exposing (Effect(..))
import HeroForge.Data.Item as Item exposing (ItemSort(..))
import HeroForge.Data.Monster as Monster exposing (Monster)
import Random exposing (Generator)
import Set.Any as AnySet exposing (AnySet)


type Direction
    = Left
    | Right


type alias Game =
    { deck : Deck Card
    , health : Int
    , maxHealth : Int
    , money : Int
    , attack : Int
    , currentLevel : Level
    , wonLevels : AnySet String Level
    }


current : Game -> Card
current =
    .deck
        >> Deck.current
        >> Maybe.withDefault Death


type Msg
    = Chosen Direction


init : Maybe { deck : Deck Card, wonLevels : AnySet String Level } -> Game
init maybeDeck =
    let
        deck =
            case maybeDeck of
                Just oldSave ->
                    oldSave.deck
                        |> Deck.findNext ((==) (Entrance Forest))

                Nothing ->
                    [ Entrance Forest
                    , Camp
                    , Endboss
                        { monster = Monster.bossBandidLeader
                        , minion = Monster.bandid
                        }
                    , Spawner (HasMaxHealth 4) <|
                        Endboss
                            { monster = Monster.bossBandidLeader
                            , minion = Monster.bandid
                            }
                    , Entrance Village
                    , Shop 5
                        { name = "Old Lether Armor"
                        , sort = Armor 2
                        , desc = ""
                        }
                    ]
                        |> Deck.fromList
    in
    { deck = deck
    , maxHealth = 3
    , health = 3
    , money = 0
    , attack = 0
    , currentLevel = Village
    , wonLevels =
        maybeDeck
            |> Maybe.map .wonLevels
            |> Maybe.withDefault (AnySet.empty Card.levelToString)
    }


check : ConditionType -> Game -> Bool
check cond game =
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

        Not c ->
            not (check c game)

        And c1 c2 ->
            check c1 game && check c2 game

        Or c1 c2 ->
            check c1 game || check c2 game

        Success ->
            True


applyEffect : Effect -> Game -> Generator Game
applyEffect effect game =
    case effect of
        Restart ->
            init
                (Just
                    { deck = game.deck
                    , wonLevels = game.wonLevels
                    }
                )
                |> Random.constant

        ExitArea ->
            { game
                | deck =
                    game.deck
                        |> Deck.findNext
                            (\c ->
                                case c of
                                    Entrance _ ->
                                        True

                                    _ ->
                                        False
                            )
            }
                |> Random.constant

        NextCard ->
            { game
                | deck =
                    game.deck
                        |> Deck.next
            }
                |> Random.constant

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

        SetCurrentLevel level ->
            { game
                | currentLevel = level
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
            let
                fight : Monster -> Card -> Generator (Deck Card)
                fight monster card =
                    game.deck
                        |> Deck.remove
                        |> (if monster.health - game.attack > 0 then
                                Deck.add card
                                    >> Random.constant

                            else
                                \deck ->
                                    Item.generateLoot
                                        (case game.currentLevel of
                                            Village ->
                                                0

                                            Forest ->
                                                0

                                            Mountains ->
                                                1
                                        )
                                        |> Random.list monster.loot
                                        |> Random.map
                                            (List.foldl
                                                (Loot >> Deck.add)
                                                deck
                                            )
                           )
            in
            case game |> current of
                Enemy monster ->
                    fight monster
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
                        |> Random.map
                            (\deck ->
                                { game
                                    | deck = deck
                                }
                            )

                Endboss { monster, minion } ->
                    fight monster
                        (Endboss
                            { monster =
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
                            , minion = minion
                            }
                        )
                        |> Random.map
                            (\deck -> { game | deck = deck })
                        |> Random.andThen
                            (\g ->
                                if
                                    (game.wonLevels
                                        |> AnySet.member game.currentLevel
                                        |> not
                                    )
                                        && (monster.health - game.attack <= 0)
                                then
                                    game.currentLevel
                                        |> CardDetails.onFreed
                                        |> List.map applyEffect
                                        |> List.foldl Random.andThen
                                            (Random.constant g)

                                else
                                    Random.constant g
                            )
                        |> Random.map
                            (\g ->
                                { g
                                    | wonLevels =
                                        game.wonLevels
                                            |> (if monster.health - game.attack <= 0 then
                                                    AnySet.insert game.currentLevel

                                                else
                                                    identity
                                               )
                                }
                            )

                _ ->
                    game |> Random.constant

        Conditional cond e ->
            if check cond game then
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
