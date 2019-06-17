module AsteroidMiner.View.Tileset exposing
    ( coloredConveyorBelt
    , comet
    , container
    , conveyorBelt
    , ground
    , mine
    , mountain
    , oreGround
    , stone
    , tileset
    , valid
    )

import AsteroidMiner.Building exposing (BeltColor(..), Code(..))
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Coord, Position)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }

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


conveyorBelt : Code -> Tile msg
conveyorBelt code =
    case code of
        Invalid ->
            Tile.fromPosition ( 0, 2 )

        InputFound ->
            Tile.fromPosition ( 1, 2 )

        Try Blue ->
            Tile.fromPosition ( 4, 6 )
        Try Green ->
                    Tile.fromPosition ( 5, 6 )
        Try Red ->
                    Tile.fromPosition ( 6, 6 )
        Try Yellow ->
                    Tile.fromPosition ( 7, 6 )
        Failed Blue ->
                    Tile.fromPosition ( 4, 7 )
        Failed Green ->
                            Tile.fromPosition ( 5, 7 )
        Failed Red ->
                            Tile.fromPosition ( 6, 7 )
        Failed Yellow ->
                            Tile.fromPosition ( 7, 7 )


coloredConveyorBelt : BeltColor -> Direction -> Tile msg
coloredConveyorBelt color dir =
    let
        getCoords : Coord
        getCoords =
            case dir of
                Right ->
                    { x = 0, y = 0 }

                Down ->
                    { x = 1, y = 0 }

                Up ->
                    { x = 2, y = 0 }

                Left ->
                    { x = 3, y = 0 }

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
    pos
        |> Position.add getCoords
        |> Tile.fromPosition


container : Tile msg
container =
    Tile.fromPosition ( 4, 3 )


stone : Tile msg
stone =
    Tile.fromPosition ( 2, 0 )
