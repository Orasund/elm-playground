module HighschoolDrama.Data exposing (Groups(..), Options, Orientation(..), Sex(..))

import Random exposing (Seed)


type Groups
    = BoysAndGirls
        { boys : Int
        , girls : Int
        , you : Sex
        }
    | AllEqual Int


type alias Options =
    { seed : Seed
    , groups : Groups
    }


type Sex
    = Male
    | Female


type Orientation
    = Hetero
    | Homosexual
    | Bi
