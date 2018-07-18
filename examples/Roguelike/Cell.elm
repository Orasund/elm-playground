module Roguelike.Cell exposing (Cell(..), ConsumableType(..), Direction(..), EffectType(..), EnemyType(..), Item(..), MiscellaneousType(..), SolidType(..), getImage)

import PixelEngine exposing (Tile, animatedTile, tile)


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
    | Enemy EnemyType
    | Item Item
    | Effect EffectType


getImage : Cell -> Tile
getImage cell =
    case cell of
        Player a ->
            case a of
                Down ->
                    animatedTile ( 12, 12 ) 1

                Up ->
                    animatedTile ( 12, 13 ) 1

                Left ->
                    animatedTile ( 12, 14 ) 1

                Right ->
                    animatedTile ( 12, 15 ) 1

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

        Enemy PlacedBombe ->
            tile ( 4, 9 )

        Enemy Rat ->
            animatedTile ( 0, 8 ) 1

        Effect Smoke ->
            tile ( 14, 14 )



--_ ->
--   ( 7, 12 )
