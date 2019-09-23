module OneSwitch.View exposing (tileset)

import OneSwitch.Data exposing (spriteSize)
import PixelEngine.Tile exposing (Tileset)


tileset : Tileset
tileset =
    { source = "tileset.png"
    , spriteWidth = spriteSize
    , spriteHeight = spriteSize
    }
