module AsteroidMiner.View.Tileset.Big exposing
    ( container
    , conveyorBelt
    )

import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 16
        , spriteHeight = 16
        }


toImage : Tile msg -> Image msg
toImage tile =
    Image.fromTile tile tileset


defaultImage : Tile msg
defaultImage =
    Tile.fromPosition ( 4, 4 )


conveyorBelt : { image : Image msg, symobl : Image msg }
conveyorBelt =
    { image =
        Tile.fromPosition ( 0, 4 )
            |> toImage
    , symobl =
        Tile.multipleTiles
            [ defaultImage
            , Tile.fromPosition ( 0, 5 )
            ]
            |> toImage
    }


container : { image : Image msg, symobl : Image msg }
container =
    { image =
        Tile.fromPosition ( 1, 4 )
            |> toImage
    , symobl =
        Tile.multipleTiles
            [ defaultImage
            , Tile.fromPosition ( 1, 5 )
            ]
            |> toImage
    }
