module  Data.Game exposing (Game, init, update)

import Dict exposing (Dict)
import  Data.Achievement as Achievement exposing (Achievement(..))
import  Data.Cell exposing (Cell(..))
import  Data.Food as Food exposing (Food(..))
import  Data.Item exposing (Item(..))
import  Data.World as World
import Random exposing (Generator)


type alias Game =
    { player : { x : Int, y : Int }
    , food : Int
    , world : Dict ( Int, Int ) Cell
    , level : Int
    , item : Maybe Item
    , achievement : Achievement
    , running : Bool
    }


worldSize : Int -> Int
worldSize level =
    5 + (level - 1) * 2


init : Generator Game
init =
    let
        level =
            1

        player =
            { x = 0
            , y = worldSize level // 2
            }
    in
    World.generate
        { worldSize = worldSize level
        , startAtY = player.y
        , level = level
        }
        |> Random.map
            (\world ->
                { player = player
                , food = 20
                , item = Nothing
                , world = world
                , level = level
                , achievement = StartedGame
                , running = False
                }
            )


rabbitUpdate :
    { player : { x : Int, y : Int }, pos : { x : Int, y : Int } }
    -> Dict ( Int, Int ) Cell
    -> Generator ( Dict ( Int, Int ) Cell, { x : Int, y : Int } )
rabbitUpdate { player, pos } dict =
    let
        ( alternativePos, priorityPos ) =
            [ ( pos.x, pos.y + 1 )
            , ( pos.x, pos.y - 1 )
            , ( pos.x + 1, pos.y )
            , ( pos.x - 1, pos.y )
            , ( pos.x + 1, pos.y + 1 )
            , ( pos.x + 1, pos.y - 1 )
            , ( pos.x - 1, pos.y + 1 )
            , ( pos.x - 1, pos.y - 1 )
            ]
                |> List.filterMap
                    (\( x, y ) ->
                        if (abs (x - player.x) <= 1) && (abs (y - player.y) <= 1) then
                            Nothing

                        else
                            case dict |> Dict.get ( x, y ) of
                                Just Ground ->
                                    Just <| ( ( x, y ), Ground )

                                Just (Food Carrot) ->
                                    Just <| ( ( x, y ), Food Carrot )

                                Just (Seed Carrot) ->
                                    Just <| ( ( x, y ), Seed Carrot )

                                Just (Plant int Carrot) ->
                                    Just <| ( ( x, y ), Plant int Carrot )

                                _ ->
                                    Nothing
                    )
                |> List.partition (Tuple.second >> (==) Ground)
                |> Tuple.mapBoth (List.map Tuple.first) (List.map Tuple.first)
    in
    case priorityPos of
        head :: tail ->
            Random.uniform head tail
                |> Random.map
                    (\( x, y ) ->
                        ( dict
                            |> Dict.insert ( x, y ) (Item Shit)
                        , pos
                        )
                    )

        [] ->
            case alternativePos of
                head :: tail ->
                    Random.uniform head tail
                        |> Random.map
                            (\( x, y ) ->
                                ( dict
                                    |> Dict.insert ( pos.x, pos.y ) Ground
                                    |> Dict.insert ( x, y ) Rabbit
                                , { x = x, y = y }
                                )
                            )

                [] ->
                    ( dict
                    , pos
                    )
                        |> Random.constant


interact : { x : Int, y : Int } -> Maybe Cell -> Game -> Result () (Generator Game)
interact { x, y } maybeCell game =
    let
        foodCell food =
            { game
                | food = game.food + (food |> Food.value)
                , world = game.world |> Dict.insert ( x, y ) Ground
                , player = { x = x, y = y }
                , achievement =
                    if
                        (food == Cherry)
                            && (game.achievement |> Achievement.smallerThen AteACherry)
                    then
                        AteACherry

                    else if
                        (food == Berry)
                            && (game.achievement |> Achievement.smallerThen AteABerry)
                    then
                        AteABerry

                    else if
                        (food == Carrot)
                            && (game.achievement |> Achievement.smallerThen AteACarrot)
                    then
                        AteACarrot

                    else if
                        (food == Melon)
                            && (game.achievement |> Achievement.smallerThen AteAMelon)
                    then
                        AteAMelon

                    else if
                        (food == Apple)
                            && (game.achievement |> Achievement.smallerThen AteAnApple)
                    then
                        AteAnApple

                    else
                        game.achievement
            }
                |> Random.constant
                |> Ok

        itemCell item =
            { game
                | item = Just item
                , world =
                    case game.item of
                        Just item2 ->
                            game.world
                                |> Dict.insert ( x, y ) Ground
                                |> Dict.insert ( game.player.x, game.player.y ) (Item item2)

                        Nothing ->
                            game.world |> Dict.insert ( x, y ) Ground
                , player = { x = x, y = y }
                , achievement =
                    if
                        item
                            == Water
                            && (game.achievement
                                    |> Achievement.smallerThen PickedUpWater
                               )
                    then
                        PickedUpWater

                    else if item == Axe && (game.achievement |> Achievement.smallerThen PickedUpAxe) then
                        PickedUpAxe

                    else
                        game.achievement
            }
                |> Random.constant
                |> Ok
    in
    case maybeCell of
        Just Ground ->
            { game
                | player = { x = x, y = y }
            }
                |> Random.constant
                |> Ok

        Just Goal ->
            let
                level =
                    game.level + 1
            in
            World.generate
                { worldSize = worldSize level
                , startAtY = worldSize level // 2
                , level = level
                }
                |> Random.map
                    (\world ->
                        { game
                            | player =
                                { y = worldSize level // 2
                                , x = 0
                                }
                            , world = world
                            , level = level
                            , achievement =
                                if (level >= 25) && (game.achievement |> Achievement.smallerThen ReachedDesert) then
                                    ReachedDesert

                                else
                                    game.achievement
                        }
                    )
                |> Ok

        Just (Food food) ->
            foodCell food

        Just (Item item) ->
            itemCell item

        Just (Seed food) ->
            case game.item of
                Just Water ->
                    { game
                        | world =
                            game.world
                                |> Dict.insert ( x, y ) (Plant (food |> Food.waitingTime) food)
                        , item = Nothing
                        , achievement =
                            if game.achievement |> Achievement.smallerThen WateredAPlant then
                                WateredAPlant

                            else
                                game.achievement
                    }
                        |> Random.constant
                        |> Ok

                Just Axe ->
                    { game
                        | world =
                            game.world
                                |> Dict.insert ( x, y ) Ground
                        , item = Nothing
                        , achievement =
                            if game.achievement |> Achievement.smallerThen ChoppedDownATree then
                                ChoppedDownATree

                            else
                                game.achievement
                    }
                        |> Random.constant
                        |> Ok

                _ ->
                    Err ()

        Just (Plant int food) ->
            if game.item == Just Shit then
                { game
                    | world =
                        game.world
                            |> Dict.insert ( x, y ) (Plant (int - 50) food)
                    , item = Nothing
                }
                    |> Random.constant
                    |> Ok

            else
                Err ()

        Just Wood ->
            if game.item == Just Axe then
                { game
                    | world =
                        game.world
                            |> Dict.insert ( x, y ) Ground
                    , player = { x = x, y = y }
                    , item = Nothing
                    , achievement =
                        if game.achievement |> Achievement.smallerThen ChoppedDownATree then
                            ChoppedDownATree

                        else
                            game.achievement
                }
                    |> Random.constant
                    |> Ok

            else
                let
                    ( dirX, dirY ) =
                        ( x - game.player.x, y - game.player.y )

                    newPos =
                        ( x + dirX, y + dirY )
                in
                case game.world |> Dict.get newPos of
                    Just Ground ->
                        { game
                            | world =
                                game.world
                                    |> Dict.insert newPos Wood
                                    |> Dict.insert ( x, y ) Ground
                            , player = { x = x, y = y }
                            , achievement =
                                if game.achievement |> Achievement.smallerThen MovedALog then
                                    MovedALog

                                else
                                    game.achievement
                        }
                            |> Random.constant
                            |> Ok

                    _ ->
                        Err ()

        Just Rabbit ->
            if game.item == Just Axe then
                { game
                    | world =
                        game.world
                            |> Dict.insert ( x, y ) (Food Meat)
                    , item = Nothing
                    , achievement =
                        if game.achievement |> Achievement.smallerThen ChoppedDownATree then
                            ChoppedDownATree

                        else
                            game.achievement
                }
                    |> Random.constant
                    |> Ok

            else
                Err ()

        Nothing ->
            --Tree
            let
                ( dirX, dirY ) =
                    ( x - game.player.x, y - game.player.y )

                newPos =
                    ( x + dirX, y + dirY )
            in
            case game.world |> Dict.get newPos of
                Just Ground ->
                    if game.item == Just Axe then
                        { game
                            | world =
                                game.world
                                    |> Dict.insert newPos Wood
                                    |> Dict.insert ( x, y ) Ground
                            , player = { x = x, y = y }
                            , item = Nothing
                            , achievement =
                                if game.achievement |> Achievement.smallerThen ChoppedDownATree then
                                    ChoppedDownATree

                                else
                                    game.achievement
                        }
                            |> Random.constant
                            |> Ok

                    else
                        Err ()

                _ ->
                    if game.item == Just Axe then
                        { game
                            | world =
                                game.world
                                    |> Dict.insert ( x, y ) Wood
                            , item = Nothing
                            , achievement =
                                if game.achievement |> Achievement.smallerThen ChoppedDownATree then
                                    ChoppedDownATree

                                else
                                    game.achievement
                        }
                            |> Random.constant
                            |> Ok

                    else
                        Err ()


tick : { x : Int, y : Int } -> Dict ( Int, Int ) Cell -> Generator (Dict ( Int, Int ) Cell)
tick player d =
    d
        |> Dict.toList
        |> List.foldl
            (\( ( x, y ), cell ) ->
                case cell of
                    Plant int food ->
                        Random.map
                            (if int <= 1 then
                                Dict.insert ( x, y ) (Food food)

                             else
                                Dict.insert ( x, y ) (Plant (int - 1) food)
                            )

                    Rabbit ->
                        Random.andThen (rabbitUpdate { pos = { x = x, y = y }, player = player })
                            >> Random.andThen
                                (\( dict, pos ) ->
                                    dict
                                        |> rabbitUpdate { pos = pos, player = player }
                                        |> Random.map Tuple.first
                                )

                    _ ->
                        identity
            )
            (Random.constant d)


update : ( Int, Int ) -> Game -> Generator Game
update ( dirX, dirY ) game =
    let
        position =
            { x = game.player.x + dirX
            , y = game.player.y + dirY
            }

        cell =
            game.world
                |> Dict.get ( position.x, position.y )
    in
    case game |> interact position cell of
        Ok generator ->
            generator
                |> Random.andThen
                    (\g ->
                        g.world
                            |> tick g.player
                            |> Random.map
                                (\world ->
                                    { g
                                        | food = g.food - 1
                                        , world = world
                                    }
                                )
                    )

        Err () ->
            Random.constant game
