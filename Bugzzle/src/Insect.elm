module Insect exposing (..)

import Gen.Enum.Natural exposing (Natural(..))


type Insect
    = Caterpillar
    | Beetle
    | Snail


type Action
    = MoveTo
    | SwapTo Natural


spawn : Natural -> Insect
spawn natural =
    case natural of
        Fruit ->
            Caterpillar

        Leaf ->
            Snail

        _ ->
            Beetle


movement : ( Int, Int ) -> ( ( Int, Int ), Maybe Natural ) -> Insect -> Bool
movement ( x, y ) ( ( posX, posY ), maybe ) insect =
    case ( maybe, insect ) of
        ( Just Fruit, Caterpillar ) ->
            True

        ( Just _, Caterpillar ) ->
            ( posX, posY ) == ( x, y - 1 )

        ( Just _, Beetle ) ->
            ( posX, posY ) == ( x, y - 1 )

        ( Nothing, Beetle ) ->
            posY <= y

        ( Just Leaf, Snail ) ->
            posY >= y

        ( Just Stone, Snail ) ->
            ( posX, posY ) == ( x, y - 1 )

        ( Just _, Snail ) ->
            ( posX, posY ) == ( x, y + 1 )

        ( _, _ ) ->
            False


move : Maybe Natural -> Insect -> Maybe Action
move tile insect =
    case ( tile, insect ) of
        ( Just natural, Caterpillar ) ->
            SwapTo natural |> Just

        ( Nothing, Caterpillar ) ->
            Nothing

        ( Just natural, Beetle ) ->
            SwapTo natural |> Just

        ( Nothing, Beetle ) ->
            Just MoveTo

        ( Just Leaf, Snail ) ->
            Just MoveTo

        ( Just natural, Snail ) ->
            SwapTo natural |> Just

        ( Nothing, Snail ) ->
            Nothing
