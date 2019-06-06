module AsteroidMiner.Tileset exposing
    ( comet
    , ground
    , mountain
    , ore_ground
    , tileset
    )

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


ore_ground : Tile msg
ore_ground =
    Tile.fromPosition ( 1, 1 )


comet : Tile msg
comet =
    Tile.fromPosition ( 4, 4 )
        |> Tile.animated 4
        |> Tile.movable "comet"
