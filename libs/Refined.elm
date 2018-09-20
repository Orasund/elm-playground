module Refined exposing (Refined, get, refine, set)


type alias Refined a =
    { value : a
    , refinement : a -> Bool
    }


refine : a -> (a -> Bool) -> Result String (Refined a)
refine value refinement =
    if refinement value then
        Ok { value = value, refinement = refinement }

    else
        Err (toString value ++ " does not satisfy the refinement!")


get : Refined a -> a
get refined =
    refined.value


set : a -> Refined a -> Result String (Refined a)
set new_value refined =
    refine new_value refined.refinement
