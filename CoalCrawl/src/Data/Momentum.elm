module Data.Momentum exposing (..)

import Data.Position


type alias Momentum =
    { momentum : Maybe ( Int, Int ) }


none : Momentum
none =
    { momentum = Nothing }


new : ( Int, Int ) -> Momentum
new pos =
    { momentum = Just pos }


fromPoints : { from : ( Int, Int ), to : ( Int, Int ) } -> Momentum
fromPoints args =
    args.from
        |> Data.Position.vecTo args.to
        |> new


revert : Momentum -> Momentum
revert { momentum } =
    { momentum =
        momentum
            |> Maybe.map
                (Tuple.mapBoth
                    ((*) -1)
                    ((*) -1)
                )
    }
