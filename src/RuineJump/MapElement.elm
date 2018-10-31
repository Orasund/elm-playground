module RuineJump.MapElement exposing (MapElement(..),Block(..), dirtGenerator, nat16Generator, toTiles)

import Natural exposing (Natural16(..))
import PixelEngine.Graphics.Tile exposing (Tile)
import Random exposing (Generator)
import RuineJump.Player exposing (FaceingDirection(..), Player, PlayerAction(..))
import RuineJump.Tileset as Tileset

type Block = Dirt

type MapElement
    = PlayerElement PlayerAction FaceingDirection
    | BlockElement Block Natural16

nat16Generator : Generator Natural16
nat16Generator =
    Random.int 0 15
        |> Random.map (Natural.fromIntTo16 >> Maybe.withDefault Zero)


dirtGenerator : Generator MapElement
dirtGenerator =
     nat16Generator |> Random.map (BlockElement Dirt)


toTiles : ( Int, Int ) -> MapElement -> List ( ( Int, Int ), Tile msg )
toTiles pos mapElement =
    case mapElement of
        PlayerElement action faceingDirection ->
            case action of
                Standing ->
                    pos
                        |> (case faceingDirection of
                                FaceingLeft ->
                                    Tileset.player_left

                                FaceingRight ->
                                    Tileset.player_right
                           )

                Falling ->
                    pos
                        |> (case faceingDirection of
                                FaceingLeft ->
                                    Tileset.player_jump_left

                                FaceingRight ->
                                    Tileset.player_jump_right
                           )

        BlockElement block id->
          case block of
            Dirt ->
              List.singleton ( pos, Tileset.dirt id )
