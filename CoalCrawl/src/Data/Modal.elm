module Data.Modal exposing (..)

import Data.Improvement exposing (Improvement)


type Modal
    = TitleModal { animationFrame : Int }
    | LevelUpModal (List Improvement)


title : Modal
title =
    { animationFrame = 0 }
        |> TitleModal


levelUp : List Improvement -> Modal
levelUp =
    LevelUpModal


timePassed : Modal -> Modal
timePassed m =
    case m of
        TitleModal modal ->
            { modal | animationFrame = modal.animationFrame + 1 }
                |> TitleModal

        LevelUpModal _ ->
            m
