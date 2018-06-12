module Roguelike.Cell exposing (Cell(..), ConsumableType(..), Direction(..), EnemyType(..), Item(..), SolidType(..), getImage)


type SolidType
    = DirtWall


type ConsumableType
    = Bombe
    | Cheese


type EnemyType
    = PlacedBombe


type Item
    = Consumable ConsumableType


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

        Item (Consumable Bombe) ->
            ( 6, 6 )

        Item (Consumable Cheese) ->
            ( 5, 7 )

        Enemy PlacedBombe ->
            ( 4, 9 )



--_ ->
--   ( 7, 12 )
