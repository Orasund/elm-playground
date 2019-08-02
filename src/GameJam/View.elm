module GameJam.View exposing (tileset)

import GameJam.Data exposing (spriteSize)
import PixelEngine.Tile exposing (Tileset)


tileset : Tileset
tileset =
    { source = "tileset.png"
    , spriteWidth = spriteSize
    , spriteHeight = spriteSize
    }
