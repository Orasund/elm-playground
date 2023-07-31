module Game.Generate exposing (..)

import Game exposing (Game)
import Level exposing (Level(..))
import Stage


new : { level : Level, stage : Int } -> Maybe Game
new args =
    case args.level of
        Level1 ->
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
                        |> Game.fromStage
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
                        |> Game.fromStage
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
                        |> Game.fromStage
                        |> Just

                _ ->
                    Nothing

        Level2 ->
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
                        |> Game.fromStage
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
                        |> Game.fromStage
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
                        |> Game.fromStage
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
                        |> Game.fromStage
                        |> Just

                _ ->
                    Nothing--}
