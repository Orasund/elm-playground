module Game exposing (..)

import Dict exposing (Dict)


type alias Game =
    { board : Dict ( Int, Int ) Int
    , width : Int
    , height : Int
    , goal : ( Int, Int )
    , tiles : List Int
    }


test : Game
test =
    { board =
        [ ( ( 0, 0 ), 1 )
        , ( ( 0, 1 ), 2 )
        , ( ( 1, 0 ), 3 )
        , ( ( 1, 1 ), 3 )
        , ( ( 0, 2 ), 4 )
        , ( ( 0, 3 ), 4 )
        , ( ( 1, 2 ), 5 )
        , ( ( 1, 3 ), 6 )
        , ( ( 2, 3 ), -1 )
        ]
            |> Dict.fromList
    , width = 3
    , height = 4
    , goal = ( 2, -1 )
    , tiles = [ -1, 1, 2, 3, 4, 5, 6 ]
    }


gameWon : Game -> Bool
gameWon game =
    game.board
        |> Dict.get game.goal
        |> (==) (Just -1)


move : Int -> Game -> Maybe Game
move targetId game =
    let
        movements =
            [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]

        isValidPos ( x, y ) =
            if
                ((targetId == -1) && (( x, y ) == game.goal))
                    || ((0 <= x && x < game.width) && (0 <= y && y < game.height))
            then
                Just ( x, y )

            else
                Nothing
    in
    movements
        |> List.filterMap
            (\( x1, y1 ) ->
                game.board
                    |> Dict.foldl
                        (\( x2, y2 ) tileId ->
                            Maybe.andThen
                                (\list ->
                                    if tileId == targetId then
                                        ( x1 + x2, y1 + y2 )
                                            |> isValidPos
                                            |> Maybe.andThen
                                                (\pos ->
                                                    if
                                                        game.board
                                                            |> Dict.get pos
                                                            |> Maybe.map ((==) targetId)
                                                            |> Maybe.withDefault True
                                                    then
                                                        pos :: list |> Just

                                                    else
                                                        Nothing
                                                )

                                    else
                                        Just list
                                )
                        )
                        (Just [])
                    |> Maybe.map
                        (\list ->
                            list
                                |> List.foldl (\pos -> Dict.insert pos targetId)
                                    (Dict.filter (\_ tileId -> tileId /= targetId) game.board)
                        )
            )
        |> (\list ->
                case list of
                    [ dict ] ->
                        { game | board = dict } |> Just

                    _ ->
                        Nothing
           )
