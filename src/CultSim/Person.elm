module CultSim.Person exposing (Action(..), Person, Position, generate, move, pray, setPraying, tile, tile_bar)

import PixelEngine.Graphics.Tile as Tile exposing (Tile)
import Random


type alias Position =
    { x : Float, y : Float }


type Action
    = Walking
    | PendingPraying
    | Praying Int
    | Dying


type alias Skin =
    { head : Int, body : Int }


type alias Person =
    { position : Position
    , action : Action
    , skin : Skin
    , praying_duration : Int
    }


tile : Action -> Tile msg
tile action =
    case action of
        PendingPraying ->
            Tile.tile ( 0, 1 )

        Praying _ ->
            Tile.tile ( 0, 2 )
        
        _ ->
            Tile.tile ( 0, 0 )



tile_bar : Int -> Tile msg
tile_bar amount =
    Tile.tile ( 0, amount )


generateSkin : Random.Generator Skin
generateSkin =
    Random.map3
        (\isMale head body ->
            { head =
                if isMale then
                    head
                else
                    head + 50
            , body = body
            }
        )
        Random.bool
        (Random.int 1 12)
        (Random.int 0 7)


generatePosition : Random.Generator Position
generatePosition =
    Random.float 0 (2 * pi)
        |> Random.map
            (\phi ->
                let
                    r : Float
                    r =
                        150

                    x : Float
                    x =
                        r * cos phi

                    y : Float
                    y =
                        r * sin phi
                in
                { x = x, y = y }
            )


move : Person -> Random.Seed -> ( Person, Random.Seed )
move person =
    Random.step generatePosition
        >> Tuple.mapFirst
            (\position ->
                { person
                    | position = position
                    , action = Walking
                }
            )


pray : Person -> Person
pray ({ praying_duration } as person) =
    { person
        | action = Praying <| praying_duration + 1
        , praying_duration = praying_duration + 1
    }


setPraying : Person -> Person
setPraying person =
    { person
        | action = PendingPraying
    }


generate : Random.Generator ( String, Person )
generate =
    Random.map3
        (\position float skin ->
            ( "person_" ++ toString float
            , { position = position
              , action = Walking
              , skin = skin
              , praying_duration = 0
              }
            )
        )
        generatePosition
        (Random.float 0 1)
        generateSkin
