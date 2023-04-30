module Game.Merge exposing (mergeTiles)

import Config
import Dict
import Game.Type exposing (Game)
import Set exposing (Set)


mergeTiles : Game -> Game
mergeTiles game =
    let
        actions =
            game
                |> calcActions
    in
    game
        |> removeAll actions.remove
        |> upgradeAll actions.upgrade


removeAll : Set ( Int, Int ) -> Game -> Game
removeAll set game =
    set
        |> Set.foldl Game.Type.removeTileAtPosition game


upgradeAll : Set ( Int, Int ) -> Game -> Game
upgradeAll set game =
    set
        |> Set.foldl
            (\pos ->
                Game.Type.updateValueAtPosition pos
                    (\int -> int + 1)
            )
            { game | minValue = game.minValue + Config.minValueIncrease }


calcHorizontalActionOf : ( Int, Int ) -> Game -> { upgrade : List ( Int, Int ), remove : List ( Int, Int ) }
calcHorizontalActionOf ( x, y ) game =
    Maybe.map3
        (\( _, value ) ( _, value1 ) ( _, value2 ) ->
            if value == value1 && value == value2 then
                { upgrade = [ ( x, y ) ]
                , remove = [ ( x - 1, y ), ( x + 1, y ) ]
                }
                    |> Just

            else
                Nothing
        )
        (Game.Type.getTileByPosition ( x, y ) game)
        (Game.Type.getTileByPosition ( x - 1, y ) game)
        (Game.Type.getTileByPosition ( x + 1, y ) game)
        |> Maybe.andThen identity
        |> Maybe.withDefault { upgrade = [], remove = [] }


calcVerticalActionOf : ( Int, Int ) -> Game -> { upgrade : List ( Int, Int ), remove : List ( Int, Int ) }
calcVerticalActionOf ( x, y ) game =
    Maybe.map3
        (\( _, value ) ( _, value1 ) ( _, value2 ) ->
            if value == value1 && value == value2 then
                { upgrade = [ ( x, y ) ]
                , remove = [ ( x, y - 1 ), ( x, y + 1 ) ]
                }
                    |> Just

            else
                Nothing
        )
        (Game.Type.getTileByPosition ( x, y ) game)
        (Game.Type.getTileByPosition ( x, y - 1 ) game)
        (Game.Type.getTileByPosition ( x, y + 1 ) game)
        |> Maybe.andThen identity
        |> Maybe.withDefault { upgrade = [], remove = [] }


joinActions : List { upgrade : List ( Int, Int ), remove : List ( Int, Int ) } -> { upgrade : Set ( Int, Int ), remove : Set ( Int, Int ) }
joinActions list =
    let
        ( upgrade, remove ) =
            list
                |> List.map (\action -> ( action.upgrade, action.remove ))
                |> List.unzip
    in
    { upgrade = upgrade |> List.concat |> Set.fromList
    , remove = remove |> List.concat |> Set.fromList
    }


calcActions : Game -> { upgrade : Set ( Int, Int ), remove : Set ( Int, Int ) }
calcActions game =
    game.grid
        |> Dict.keys
        |> List.concatMap
            (\pos ->
                [ calcHorizontalActionOf pos game
                , calcVerticalActionOf pos game
                ]
            )
        |> joinActions
        |> (\acc ->
                let
                    upgrade =
                        acc.upgrade

                    remove =
                        Set.diff acc.remove upgrade
                in
                { upgrade = upgrade
                , remove = remove
                }
           )
