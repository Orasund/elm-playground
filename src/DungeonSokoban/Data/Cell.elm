module DungeonSokoban.Data.Cell exposing (Cell(..), pushable)


type Cell
    = Hole
    | Monster { stunned : Bool }
    | Box


{-| If pushable returns the resulting cell
-}
pushable : Cell -> Maybe Cell
pushable cell =
    case cell of
        Hole ->
            Nothing

        Monster _ ->
            Monster { stunned = True } |> Just

        Box ->
            Box |> Just
