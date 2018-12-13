module RuineJump.MapElement exposing (Block(..), MapElement(..), dirtGenerator, toTiles)

import Natural exposing (Natural16(..))
import PixelEngine.Graphics.Tile exposing (Tile)
import Random exposing (Generator)
import RuineJump.Player exposing (FaceingDirection(..), Player, PlayerAction(..))
import RuineJump.Tileset as Tileset


type Block
    = Dirt
    | Grass
    | Stone
    | Air


type MapElement
    = PlayerElement PlayerAction FaceingDirection
    | BlockElement Block Int



{- nat16Generator : Generator Natural16
   nat16Generator =
       Random.int 0 15
           |> Random.map (Natural.fromIntTo16 >> Maybe.withDefault Zero)
-}


dirtGenerator : Generator MapElement
dirtGenerator =
    Random.int 0 Random.maxInt |> Random.map (BlockElement Dirt)


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

        BlockElement block id ->
            List.singleton
                ( pos
                , id
                    |> case block of
                        Dirt ->
                            Tileset.dirt
                        
                        Grass ->
                            Tileset.grass
                        
                        Stone ->
                            Tileset.stone
                        
                        Air ->
                            Tileset.air
                )

