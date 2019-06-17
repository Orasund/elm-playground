module AsteroidMiner.View.Map exposing (view)

import AsteroidMiner.Building as Building exposing (BeltColor(..), BuildingType(..))
import AsteroidMiner.Data.Game as Game
import AsteroidMiner.Data.Map exposing (GroundType(..), Item(..), Map, Square)
import AsteroidMiner.Lib.Map exposing (SquareType(..))
import AsteroidMiner.View.GUI as GUI
import AsteroidMiner.View.Tileset as Tileset
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import PixelEngine.Tile as Tile exposing (Tile)


viewSquare : { position : Position, onClick : Position -> msg, valid : Maybe Bool } -> Square -> Tile msg
viewSquare { position, onClick, valid } ( squareType, maybeItem ) =
    let
        square : Tile msg
        square =
            case squareType of
                GroundSquare groundType ->
                    case groundType of
                        Empty ->
                            Tileset.ground

                        Mountain ->
                            Tileset.mountain

                        OreGround ->
                            Tileset.oreGround

                BuildingSquare buildingType ->
                    case buildingType.sort of
                        Mine ->
                            Tileset.mine

                        ConveyorBelt code ->
                            Tileset.conveyorBelt code

                        ColoredConveyorBelt color direction ->
                            Tileset.coloredConveyorBelt color direction

                        Container ->
                            Tileset.container

        item : Maybe (Tile msg)
        item =
            maybeItem
                |> Maybe.map
                    (\i ->
                        case i of
                            Stone ->
                                Tileset.stone
                    )
    in
    (case item of
        Just tile ->
            Tile.multipleTiles
                [ square, tile ]

        Nothing ->
            square
    )
        |> (case valid of
                Just bool ->
                    \t ->
                        if bool then
                            Tile.multipleTiles [ t, Tileset.valid ]
                                |> Tile.clickable (onClick position)

                        else
                            t

                Nothing ->
                    Tile.clickable (onClick position)
           )


view : { onClick : Position -> msg, selected : GUI.Tool } -> Map -> List ( Position, Tile msg )
view { onClick, selected } map =
    map
        |> Grid.map
            (\pos ->
                Maybe.map <|
                    viewSquare
                        { position = pos
                        , onClick = onClick
                        , valid =
                            if (selected |> Building.toolToBuilding) == Nothing then
                                Nothing

                            else
                                Just <| Game.isValid selected pos map
                        }
            )
        >> Grid.toList
