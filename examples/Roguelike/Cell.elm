module Roguelike.Cell exposing (Cell(..), ConsumableType(..), Direction(..), EffectType(..), EnemyType(..), Item(..), MiscellaneousType(..), SolidType(..), getImage)

import PixelEngine exposing (Tile, animatedMovableTile, animatedTile, movableTile, tile)


type SolidType
    = DirtWall
    | PlacedDirt


type ConsumableType
    = Bombe
    | Cheese
    | Dirt


type MiscellaneousType
    = Bone


type EnemyType
    = PlacedBombe
    | Rat


type EffectType
    = Smoke


type Item
    = Consumable ConsumableType
    | Miscellaneous MiscellaneousType


type Direction
    = Up
    | Down
    | Left
    | Right


type Cell
    = Player Direction
    | Solid SolidType
    | Enemy EnemyType String
    | Item Item
    | Effect EffectType


getImage : Cell -> Tile
getImage cell =
    case cell of
        Player a ->
            case a of
                Down ->
                    animatedMovableTile ( 12, 12 ) 1 "player"

                Up ->
                    animatedMovableTile ( 12, 13 ) 1 "player"

                Left ->
                    animatedMovableTile ( 12, 14 ) 1 "player"

                Right ->
                    animatedMovableTile ( 12, 15 ) 1 "player"

        Solid DirtWall ->
            tile ( 0, 2 )

        Solid PlacedDirt ->
            tile ( 0, 3 )

        Item (Consumable Bombe) ->
            tile ( 6, 6 )

        Item (Consumable Cheese) ->
            tile ( 5, 7 )

        Item (Consumable Dirt) ->
            tile ( 8, 7 )

        Item (Miscellaneous Bone) ->
            tile ( 0, 10 )

        Enemy PlacedBombe _ ->
            animatedTile ( 4, 9 ) 1

        Enemy Rat id ->
            animatedMovableTile ( 0, 8 ) 1 id

        Effect Smoke ->
            tile ( 14, 14 )



--_ ->
--   ( 7, 12 )
