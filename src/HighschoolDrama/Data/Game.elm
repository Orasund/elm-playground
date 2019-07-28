module HighschoolDrama.Data.Game exposing (Game)

import HighschoolDrama.Data exposing (Options, Person, Sex)
import HighschoolDrama.Data.Person exposing (Person)
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
