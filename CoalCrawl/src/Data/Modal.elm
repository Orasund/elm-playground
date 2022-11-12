module Data.Modal exposing (..)

import Data.Animation exposing (Animation)


type alias Modal =
    { animationFrame : Int
    , animation : Animation
    }


fromAnimation : Animation -> Modal
fromAnimation animation =
    { animation = animation
    , animationFrame = 0
    }


timePassed : Modal -> Modal
timePassed modal =
    { modal | animationFrame = modal.animationFrame + 1 }
