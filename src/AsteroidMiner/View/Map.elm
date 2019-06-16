module AsteroidMiner.View.Map exposing (view)

import AsteroidMiner.Data.Building exposing (BeltColor(..), BuildingType(..))
import AsteroidMiner.Data.Game as Game exposing (GroundType(..), Map, Square)
import AsteroidMiner.Data.Map exposing (SquareType(..))
import AsteroidMiner.View.GUI as GUI
import AsteroidMiner.View.Tileset as Tileset
import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import PixelEngine.Tile as Tile exposing (Tile)


viewOverlay : Bool -> Tile msg
viewOverlay bool =
    case bool of
        True ->
            Tileset.valid

        False ->
            Tileset.invalid


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
                        ConveyorBelt Nothing ->
                            Tileset.conveyorBeltUncolored buildingType.counter
                        ConveyorBelt (Just color) ->
                            Tileset.conveyorBelt color


                        Container ->
                            Tileset.container

        item : Maybe (Tile msg)
        item =
            case maybeItem of
                Just i ->
                    Debug.todo "items can't be displayed"

                Nothing ->
                    Nothing
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
                        Tile.multipleTiles [ t, viewOverlay bool ]
                            |> (if bool then
                                    Tile.clickable (onClick position)

                                else
                                    identity
                               )

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
                            if
                                (selected == GUI.ConveyorBelt)
                                    || (selected == GUI.Delete)
                            then
                                Nothing

                            else
                                Just <| Game.isValid selected pos map
                        }
            )
        >> Grid.toList
