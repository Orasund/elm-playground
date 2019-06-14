module AsteroidMiner.View.Map exposing (view)

import AsteroidMiner.Data.Building exposing (BuildingType(..))
import AsteroidMiner.Data.Game exposing (GroundType(..), Map, Square)
import AsteroidMiner.Data.Map exposing (SquareType(..))
import AsteroidMiner.View.Tileset as Tileset
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import PixelEngine.Tile as Tile exposing (Tile)


viewSquare : { position : Position, onClick : Position -> msg } -> Square -> Tile msg
viewSquare { position, onClick } ( squareType, maybeItem ) =
    (case squareType of
        GroundSquare groundType ->
            case groundType of
                Empty ->
                    Tileset.ground
                        |> Tile.clickable
                            (onClick position)

                Mountain ->
                    Tileset.mountain
                        |> Tile.clickable
                            (onClick position)

                OreGround ->
                    Tileset.oreGround

        BuildingSquare buildingType ->
            case buildingType.sort of
                Mine ->
                    Tileset.mine

                ConveyorBelt maybeBeltColor ->
                    Tileset.conveyorBelt maybeBeltColor

                Container ->
                    Tileset.container
    )
        |> (case maybeItem of
                Just _ ->
                    Debug.todo "items can't be displayed"

                Nothing ->
                    identity
           )


view : (Position -> msg) -> Map -> List ( Position, Tile msg )
view onClick =
    Grid.map (\pos -> Maybe.map <| viewSquare { position = pos, onClick = onClick })
        >> Grid.toList
