module CultSim.Person exposing (Person, Position, generate, move)

import Random


type alias Position =
    { x : Float, y : Float }


type alias Person =
    { position : Position
    , id : String
    , skin : Int
    }


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
                }
            )


generate : Random.Generator Person
generate =
    Random.map2
        (\position float ->
            { position = position
            , id = "person_" ++ toString float
            , skin = 0
            }
        )
        generatePosition
    <|
        Random.float 0 1
