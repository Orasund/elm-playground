module AsteroidMiner.View.Tileset.Big exposing
    ( container
    , conveyorBelt
    , delete
    , merger
    , mine
    , pickUp
    )

import AsteroidMiner.Data exposing (spriteSize)
import AsteroidMiner.View.Tileset as Tileset
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


delete : { image : Image msg, symobl : Image msg }
delete =
    { image =
        Tile.fromPosition ( 3, 6 )
            |> toImage
    , symobl =
        Tile.multipleTiles
            [ defaultImage
            , Tile.fromPosition ( 3, 7 )
            ]
            |> toImage
    }


pickUp : Maybe (Tile msg) -> { image : Image msg, symobl : Image msg }
pickUp maybeTile =
    case maybeTile of
        Just tile ->
            { image =
                Image.multipleImages
                    [ ( ( 0, 0 ), Tile.fromPosition ( 2, 6 ) |> toImage )
                    , ( ( spriteSize / 2, 2 + spriteSize / 2 )
                      , Image.fromTile tile Tileset.tileset
                      )
                    ]
            , symobl =
                Tile.multipleTiles
                    [ defaultImage
                    , Tile.fromPosition ( 2, 7 )
                    ]
                    |> toImage
            }

        Nothing ->
            { image =
                Tile.fromPosition ( 2, 6 )
                    |> toImage
            , symobl =
                Tile.multipleTiles
                    [ defaultImage
                    , Tile.fromPosition ( 2, 7 )
                    ]
                    |> toImage
            }


mine : { image : Image msg, symobl : Image msg }
mine =
    { image =
        Tile.fromPosition ( 0, 6 )
            |> toImage
    , symobl =
        Tile.multipleTiles
            [ defaultImage
            , Tile.fromPosition ( 0, 7 )
            ]
            |> toImage
    }


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


merger : { image : Image msg, symobl : Image msg }
merger =
    { image =
        Tile.fromPosition ( 2, 4 )
            |> toImage
    , symobl =
        Tile.multipleTiles
            [ defaultImage
            , Tile.fromPosition ( 2, 5 )
            ]
            |> toImage
    }
