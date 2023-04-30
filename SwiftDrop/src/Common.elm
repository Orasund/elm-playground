module Common exposing (..)

import Config


grid : List ( Int, Int )
grid =
    List.range 0 (Config.size - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (Config.size - 1)
                    |> List.map
                        (\x -> ( x, y ))
            )
