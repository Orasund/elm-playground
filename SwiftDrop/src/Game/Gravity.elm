module Game.Gravity exposing (apply)

import Config
import Dict exposing (Dict)
import Game.Type exposing (Game)


apply : Game -> Game
apply game =
    List.range 0 (Config.size - 2)
        |> List.foldr
            (\int ->
                applyToRow int
            )
            game.grid
        |> (\grid -> { game | grid = grid })


applyToRow : Int -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
applyToRow y game =
    List.range 0 (Config.size - 1)
        |> List.foldl
            (\x ->
                applyToPosition ( x, y )
            )
            game


applyToPosition : ( Int, Int ) -> Dict ( Int, Int ) a -> Dict ( Int, Int ) a
applyToPosition ( x, y ) dict =
    if y == Config.size - 1 then
        dict

    else
        case dict |> Dict.get ( x, y + 1 ) of
            Nothing ->
                dict
                    |> Dict.get ( x, y )
                    |> Maybe.map
                        (\a ->
                            dict
                                |> Dict.insert ( x, y + 1 ) a
                                |> Dict.remove ( x, y )
                        )
                    |> Maybe.withDefault dict
                    |> applyToPosition ( x, y + 1 )

            Just _ ->
                dict
