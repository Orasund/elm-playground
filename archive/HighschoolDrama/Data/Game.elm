module HighschoolDrama.Data.Game exposing (Game, init)

import HighschoolDrama.Data exposing (Options)
import HighschoolDrama.Data.Person as Person exposing (Person)
import Random exposing (Seed)
import Set exposing (Set)


type alias Game =
    { class : Set Person
    , you : Person
    , seed : Seed
    }


init : Options -> Game
init { sex, seed } =
    { seed = seed
    , class =
        List.repeat 19 ()
            |> List.map
                (always <|
                    case sex of
                        Nothing ->
                            Person.npc { includeSex = False }

                        _ ->
                            Person.npc { includeSex = True }
                )
    , you = Person.player sex
    }
