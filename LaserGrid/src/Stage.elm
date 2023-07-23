module Stage exposing (..)

import Cell exposing (Cell(..))
import Dict exposing (Dict)


type alias Stage a =
    { grid : Dict ( Int, Int ) (Cell a)
    , targets : List ( Int, Int )
    }


targets : Dict ( Int, Int ) (Cell a) -> List ( Int, Int )
targets dict =
    dict
        |> Dict.filter
            (\_ cell ->
                case cell of
                    Target _ ->
                        True

                    _ ->
                        False
            )
        |> Dict.keys
