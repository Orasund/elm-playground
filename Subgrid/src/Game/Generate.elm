module Game.Generate exposing (..)

import Game exposing (Game)
import Stage


new : { level : Int, stage : Int } -> Maybe Game
new args =
    case args.level of
        1 ->
            case args.stage of
                3 ->
                    Stage.parse
                        [ "⬛⬛⬛⬛🟥⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛🔘⬛⬛🟥⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                2 ->
                    Stage.parse
                        [ "⬛🟥⬛⬛🟥⬛"
                        , "⬛⬜⬜⬜⬜🔘"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "⬛🔘⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                1 ->
                    Stage.parse
                        [ "⬛🔘⬛⬛🟥⬛"
                        , "🟥⬜⬜⬜⬜🔘"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛🟥⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                _ ->
                    Nothing

        2 ->
            case args.stage of
                3 ->
                    Stage.parse
                        [ "⬛⬛⬛⬛🔘⬛"
                        , "🟥⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜🟥"
                        , "⬛🔘⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                2 ->
                    Stage.parse
                        [ "⬛🟥⬛⬛🔘⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜🔘"
                        , "⬛🟥⬛⬛⬛⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                1 ->
                    Stage.parse
                        [ "⬛🟥⬛⬛🟥⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬛⬛⬛🔘⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                _ ->
                    Nothing

        {--3 ->
            case args.stage of
                1 ->
                    Stage.parse
                        [ "⬛🟥⬛⬛🟥⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "⬛⬜⬜⬜⬜⬛"
                        , "🔘⬜⬜⬜⬜⬛"
                        , "⬛⬛⬛⬛🔘⬛"
                        ]
                        |> Game.fromStage { level = args.level }
                        |> Just

                _ ->
                    Nothing--}
        _ ->
            Nothing
