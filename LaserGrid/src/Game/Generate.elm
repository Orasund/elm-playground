module Game.Generate exposing (..)

import Game exposing (Game)
import Stage


fromId : Int -> Maybe Game
fromId stageId =
    case stageId of
        6 ->
            Stage.parse
                [ "⬛⬛⬛⬛🔘⬛"
                , "🟥⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜🟥"
                , "⬛🔘⬛⬛⬛⬛"
                ]
                |> Game.fromStage { level = 2 }
                |> Just

        5 ->
            Stage.parse
                [ "⬛🟥⬛⬛🔘⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜🔘"
                , "⬛🟥⬛⬛⬛⬛"
                ]
                |> Game.fromStage { level = 2 }
                |> Just

        4 ->
            Stage.parse
                [ "⬛🟥⬛⬛🟥⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬛⬛⬛🔘⬛"
                ]
                |> Game.fromStage { level = 2 }
                |> Just

        3 ->
            Stage.parse
                [ "⬛⬛⬛⬛🟥⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛🔘⬛⬛🟥⬛"
                ]
                |> Game.fromStage { level = 1 }
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
                |> Game.fromStage { level = 1 }
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
                |> Game.fromStage { level = 1 }
                |> Just

        _ ->
            Nothing
