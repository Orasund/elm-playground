module AsteroidMiner.View.Tileset exposing
    ( comet
    , container
    , conveyorBelt
    , conveyorBeltUncolored
    , ground
    , invalid
    , mine
    , mountain
    , oreGround
    , tileset
    , valid
    )

import AsteroidMiner.Data.Building exposing (BeltColor(..))
import Grid.Direction exposing (Direction(..))
import PixelEngine.Tile as Tile exposing (Tile, Tileset)
import Grid.Position as Position exposing (Coord,Position)

tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }


invalid : Tile msg
invalid =
    Tile.fromPosition ( 2, 2 )


valid : Tile msg
valid =
    Tile.fromPosition ( 4, 5 )
        |> Tile.animated 4


ground : Tile msg
ground =
    Tile.fromPosition ( 1, 0 )


mountain : Tile msg
mountain =
    Tile.fromPosition ( 0, 1 )


oreGround : Tile msg
oreGround =
    Tile.fromPosition ( 1, 1 )


comet : Tile msg
comet =
    Tile.fromPosition ( 4, 4 )
        |> Tile.animated 4
        |> Tile.movable "comet"


mine : Tile msg
mine =
    Tile.fromPosition ( 4, 2 )
        |> Tile.animated 4


conveyorBeltUncolored : Int -> Tile msg
conveyorBeltUncolored code =
    case code of
        0 ->
            Tile.fromPosition ( 0, 2 )

        1 ->
            Tile.fromPosition ( 1, 2 )

        2 ->
            Tile.fromPosition ( 0, 3 )

        3 ->
            Tile.fromPosition ( 1, 3 )

        4 ->
            Tile.fromPosition ( 0, 3 )

        5 ->
            Tile.fromPosition ( 1, 3 )

        6 ->
            Tile.fromPosition ( 0, 3 )

        7 ->
            Tile.fromPosition ( 1, 3 )

        8 ->
            Tile.fromPosition ( 0, 3 )

        9 ->
            Tile.fromPosition ( 1, 3 )

        _ ->
            Tile.fromPosition ( 1, 3 )


conveyorBelt : (BeltColor,Direction) -> Tile msg
conveyorBelt (color,dir) =
    let
        getCoords : Coord
        getCoords =
            case dir of
                Right ->
                    {x=0,y=0}
                Down ->
                    {x=1,y=0}
                Up ->
                    {x = 2,y=0}
                Left ->
                    {x=3,y=0}

        pos : Position
        pos =
            case color of
                      Blue ->
                          ( 0, 4 )

                      Green ->
                          ( 0, 5 )

                      Red ->
                          ( 0, 6 )

                      Yellow ->
                          ( 0, 7 )

    in
    pos |> Position.add getCoords
    |>
    Tile.fromPosition


container : Tile msg
container =
    Tile.fromPosition ( 4, 3 )
