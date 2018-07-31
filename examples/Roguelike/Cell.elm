module Roguelike.Cell
    exposing
        ( Cell(..)
        , ConsumableType(..)
        , EffectType(..)
        , EnemyType(..)
        , Item(..)
        , MaterialType(..)
        , MiscellaneousType(..)
        , SolidType
        , composing
        , decomposing
        , getImage
        , mapGenerator
        , resistancy
        )

import Dict
import PixelEngine.Graphics.Tile exposing (Tile)
import Random
import Roguelike.Map exposing (Direction(..), Location, Map)
import Roguelike.Tileset as Tileset


type ConsumableType
    = Bombe
    | HealthPotion
    | Material MaterialType


type MiscellaneousType
    = Bone


type EnemyType
    = PlacedBombe
    | Oger
    | Goblin
    | Rat


type EffectType
    = Smoke


type Item
    = Consumable ConsumableType
    | Miscellaneous MiscellaneousType


type Cell
    = Player Direction
    | Solid SolidType
    | Enemy EnemyType String
    | Item Item
    | Effect EffectType


type MaterialType
    = Dirt
    | Stone


type SolidType
    = StoneWall
    | StoneBrickWall
    | Placed MaterialType


decomposing : SolidType -> ( Maybe SolidType, MaterialType )
decomposing solidType =
    case solidType of
        Placed material ->
            ( Nothing, material )

        StoneWall ->
            ( Just (Placed Dirt), Stone )

        StoneBrickWall ->
            ( Just StoneWall, Stone )


composing : ( Maybe SolidType, MaterialType ) -> Maybe SolidType
composing tuple =
    case tuple of
        ( Just (Placed placedMaterial), material ) ->
            case ( placedMaterial, material ) of
                ( Dirt, Dirt ) ->
                    Nothing

                ( Stone, Dirt ) ->
                    Just StoneWall

                ( Dirt, Stone ) ->
                    Just StoneWall

                ( Stone, Stone ) ->
                    Just StoneBrickWall

        ( Nothing, material ) ->
            Just (Placed material)

        ( Just _, _ ) ->
            Nothing


getImage : Cell -> Tile msg
getImage cell =
    case cell of
        Player a ->
            case a of
                Down ->
                    Tileset.player_down

                Up ->
                    Tileset.player_up

                Left ->
                    Tileset.player_left

                Right ->
                    Tileset.player_right

        Solid (Placed Stone) ->
            Tileset.placed_stone

        Solid (Placed Dirt) ->
            Tileset.dirt_wall

        Solid StoneWall ->
            Tileset.stone_wall

        Solid StoneBrickWall ->
            Tileset.stone_brick_wall

        Item (Consumable Bombe) ->
            Tileset.bombe

        Item (Consumable HealthPotion) ->
            Tileset.health_potion

        Item (Consumable (Material Dirt)) ->
            Tileset.dirt

        Item (Consumable (Material Stone)) ->
            Tileset.stone

        Item (Miscellaneous Bone) ->
            Tileset.bone

        Enemy PlacedBombe _ ->
            Tileset.placed_bombe

        Enemy Oger id ->
            Tileset.oger id

        Enemy Goblin id ->
            Tileset.goblin id

        Enemy Rat id ->
            Tileset.rat id

        Effect Smoke ->
            Tileset.smoke



--_ ->
--   ( 7, 12 )


resistancy : SolidType -> Int
resistancy solid =
    case solid of
        StoneWall ->
            3

        StoneBrickWall ->
            4

        Placed Dirt ->
            2

        Placed Stone ->
            2


mapGenerator : Location -> ( Map Cell, Random.Seed ) -> ( Map Cell, Random.Seed )
mapGenerator pos ( map, seed ) =
    let
        ( r, new_seed ) =
            Random.step (Random.int 0 500) seed
    in
    if
        (pos |> Tuple.first)
            == 0
            || (pos |> Tuple.second)
            == 0
            || (pos |> Tuple.first)
            == 15
            || (pos |> Tuple.second)
            == 15
    then
        ( map |> Dict.insert pos (Solid StoneBrickWall)
        , seed
        )
    else if r < 50 then
        ( map |> Dict.insert pos (Solid (Placed Dirt))
        , new_seed
        )
    else if r < 150 then
        ( map |> Dict.insert pos (Solid StoneWall)
        , new_seed
        )
    else if r < 200 then
        ( map |> Dict.insert pos (Solid StoneBrickWall)
        , new_seed
        )
    else if r < 225 then
        ( map |> Dict.insert pos (Item (Consumable Bombe))
        , new_seed
        )
    else if r < 230 then
        ( map |> Dict.insert pos (Item (Consumable HealthPotion))
        , new_seed
        )
    else if r < 235 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Rat ("Rat" ++ toString id))
        , new_seed
        )
    else if r < 238 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Goblin ("Goblin" ++ toString id))
        , new_seed
        )
    else if r < 239 then
        let
            ( id, _ ) =
                Random.step (Random.float 0 1) new_seed
        in
        ( map |> Dict.insert pos (Enemy Oger ("Oger" ++ toString id))
        , new_seed
        )
    else
        ( map
        , new_seed
        )
