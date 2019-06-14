module AsteroidMiner.Data.Neighborhood exposing (Neighborhood, fromPosition)

import Grid.Bordered as Grid exposing (Error, Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)


type alias Neighborhood a =
    { up : a
    , left : a
    , right : a
    , down : a
    }


fromPosition : Position -> Grid a -> Result Error ( Maybe a, Neighborhood (Maybe a) )
fromPosition pos grid =
    let
        get : Direction -> Maybe a
        get direction =
            grid
                |> Grid.get (pos |> Position.move 1 direction)
                |> Result.withDefault Nothing
    in
    grid
        |> Grid.get pos
        |> Result.andThen
            (\a ->
                Ok
                    ( a
                    , { up = get Up
                      , left = get Left
                      , right = get Right
                      , down = get Down
                      }
                    )
            )
