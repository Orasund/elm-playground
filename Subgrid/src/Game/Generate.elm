module Game.Generate exposing (..)

import Game exposing (Game)
import Level exposing (Level, LevelAmount)
import Stage
import StaticArray exposing (StaticArray)


levels : StaticArray LevelAmount (Int -> Maybe Game)
levels =
    ( \stage ->
        case stage of
            1 ->
                --simplest pattern
                Stage.parse
                    [ "⬛🔘⬛⬛🔘⬛"
                    , "🟥⬜⬜⬜⬜⬛"
                    , "⬛⬜⬜⬜⬜⬛"
                    , "⬛⬜⬜⬜⬜⬛"
                    , "🟥⬜⬜⬜⬜⬛"
                    , "⬛⬛⬛⬛⬛⬛"
                    ]
                    |> Game.fromStage
                    |> Just

            _ ->
                Nothing
    , [ \stage ->
            case stage of
                1 ->
                    --again a curve
                    --no alternative solutions
                    Stage.parse
                        [ "⬛🟥⬛⬛🔘⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "⬛⬛⬛⬛🟥⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                2 ->
                    --only way how to make a straight with just a curve
                    --again no alternatives
                    Stage.parse
                        [ "⬛⬛⬛⬛⬛⬛"
                        , "🔘⬜⬜⬜⬜🟥"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜🟥"
                        , "⬛🟥⬛⬛🔘⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                _ ->
                    Nothing
      , \stage ->
            case stage of
                1 ->
                    --player learns that lasers will prefer straight lines
                    Stage.parse
                        [ "⬛🟥⬛⬛🔘⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "⬛⬛⬛⬛🟥⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                2 ->
                    --player is forced to cross lines
                    Stage.parse
                        [ "⬛🔘⬛⬛🟥⬛"
                        , "🔘⬜⬜⬜⬜🟥"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛🟥⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                3 ->
                    --player will not be able to make a straight line
                    Stage.parse
                        [ "⬛🔘⬛⬛🟥⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🟥"
                        , "⬛⬛⬛⬛🔘⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                _ ->
                    Nothing
      , \stage ->
            case stage of
                3 ->
                    Stage.parse
                        [ "⬛⬛⬛🔘⬛⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "🔘⬜⬜⬜⬜🟥"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬛🟥⬛⬛⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                2 ->
                    Stage.parse
                        [ "⬛⬛🟥🔘⬛⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "🔘⬜⬜⬜⬜🟥"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬛⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                1 ->
                    Stage.parse
                        [ "⬛⬛🔘🟥⬛⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "🟥⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬛🔘⬛⬛⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                _ ->
                    Nothing
      , \stage ->
            case stage of
                1 ->
                    Stage.parse
                        [ "⬛🟥⬛⬛🟥⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬛⬛⬛🔘⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                2 ->
                    Stage.parse
                        [ "⬛🟥⬛⬛🟥⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬛⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage
                        |> Just

                _ ->
                    Nothing
      ]
    )
        |> StaticArray.fromList Level.maxLevel


new : { level : Level, stage : Int } -> Maybe Game
new args =
    levels
        |> StaticArray.get args.level
        |> (\fun -> fun args.stage)
