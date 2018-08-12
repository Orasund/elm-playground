module CultSim.Person exposing (Action(..), Person, Position, generate, update,move, setPraying)

import Random


type alias Position =
    { x : Float, y : Float }


type Action
    = Walking
    | PendingPraying
    | Praying


type alias Person =
    { position : Position
    , action : Action
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


update : Person -> Random.Seed -> ( Person, Random.Seed )
update ({ action } as person) =
    case action of
        PendingPraying ->
            (,) <| pray person

        _ ->
            move person


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
pray person =
    { person
        | action = Praying
    }


setPraying : Person -> Person
setPraying person =
    { person
        | action = PendingPraying
    }


generate : Random.Generator ( String, Person )
generate =
    Random.map2
        (\position float ->
            ( "person_" ++ toString float
            , { position = position
              , action = Walking
              }
            )
        )
        generatePosition
    <|
        Random.float 0 1
