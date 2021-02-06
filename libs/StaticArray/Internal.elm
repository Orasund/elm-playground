module StaticArray.Internal exposing (Index(..), Length(..), StaticArray(..))

import Array exposing (Array)


type Length n
    = C Int


type Index n
    = I Int


type StaticArray n a
    = A ( a, Array a )
