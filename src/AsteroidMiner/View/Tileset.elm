module AsteroidMiner.View.Tileset exposing
    ( comet
    , container
    , conveyorBelt
    , ground
    , mine
    , mountain
    , oreGround
    , tileset
    )

import AsteroidMiner.Data.Building exposing (BeltColor(..))
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }


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


conveyorBelt : Maybe BeltColor -> Tile msg
conveyorBelt maybeDir =
    case maybeDir of
        Nothing ->
            Tile.fromPosition ( 0, 2 )

        Just Blue ->
            Tile.fromPosition ( 0, 4 ) |> Tile.animated 4

        Just Green ->
            Tile.fromPosition ( 0, 5 ) |> Tile.animated 4

        Just Red ->
            Tile.fromPosition ( 0, 6 ) |> Tile.animated 4

        Just Yellow ->
            Tile.fromPosition ( 0, 7 ) |> Tile.animated 4


container : Tile msg
container =
    Tile.fromPosition ( 4, 3 )
