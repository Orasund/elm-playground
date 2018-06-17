module Roguelike.Cell exposing (Cell(..), ConsumableType(..), Direction(..), EffectType(..), EnemyType(..), Item(..), MiscellaneousType(..), SolidType(..), getImage)


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


getImage : Cell -> ( Int, Int )
getImage cell =
    case cell of
        Player a ->
            case a of
                Down ->
                    ( 12, 12 )

                Up ->
                    ( 13, 12 )

                Left ->
                    ( 13, 13 )

                Right ->
                    ( 12, 13 )

        Solid DirtWall ->
            ( 0, 2 )

        Solid PlacedDirt ->
            ( 0, 3 )

        Item (Consumable Bombe) ->
            ( 6, 6 )

        Item (Consumable Cheese) ->
            ( 5, 7 )

        Item (Consumable Dirt) ->
            ( 8, 7 )

        Item (Miscellaneous Bone) ->
            ( 12, 15 )

        Enemy PlacedBombe ->
            ( 4, 9 )

        Enemy Rat ->
            ( 0, 8 )

        Effect Smoke ->
            ( 14, 14 )



--_ ->
--   ( 7, 12 )
