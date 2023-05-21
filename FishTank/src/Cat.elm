module Cat exposing (..)

import Config
import Dict exposing (Dict)
import Fish.Common exposing (BitColor(..))
import Pigment exposing (Pigment)
import Rule exposing (Pattern)
import Set exposing (Set)


type alias Cat =
    { pattern : Set ( Int, Int )
    , primary : Pigment
    , secondary : Pigment
    }


default : Cat
default =
    { pattern = Set.empty
    , primary = { blue = True, red = True, yellow = True }
    , secondary = { blue = False, red = True, yellow = True }
    }


sprite : Dict ( Int, Int ) Bool
sprite =
    let
        x =
            1
    in
    [ [ 0, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0, x, x, x, 0 ]
    , [ 0, x, 2, 2, x, 0, 0, 0, 0, 0, 0, x, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, 2, x, 0, 0, 0, 0, x, 2, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, 2, 2, x, x, x, x, 2, 2, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, 2, x, 2, 2, 2, 2, x, 2, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, x, 2, x, 2, 2, x, 2, x, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, x, 2, 2, 2, 2, 2, 2, x, 2, 2, x, 0 ]
    , [ 0, x, 2, 2, x, 2, 2, x, x, 2, 2, x, 2, 2, x, 0 ]
    , [ 0, 0, x, 2, 2, x, x, 2, 2, x, x, 2, 2, x, 0, 0 ]
    , [ 0, 0, x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x, 0, 0 ]
    , [ 0, 0, 0, x, 2, 2, 2, 2, 2, 2, 2, 2, x, 0, 0, 0 ]
    , [ 0, 0, 0, 0, x, x, 2, 2, 2, 2, x, x, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, x, x, x, x, 0, 0, 0, 0, 0, 0 ]
    ]
        |> List.indexedMap
            (\y list ->
                list
                    |> List.indexedMap
                        (\i_x value ->
                            ( ( i_x, y ), value )
                        )
                    |> List.filter
                        (\( _, value ) ->
                            value /= 0
                        )
            )
        |> List.concat
        |> Dict.fromList
        |> Dict.map
            (\_ int ->
                case int of
                    1 ->
                        False

                    _ ->
                        True
            )


toBitmap : Set ( Int, Int ) -> List (List BitColor)
toBitmap set =
    List.repeat Config.spriteSize ()
        |> List.indexedMap
            (\y () ->
                List.repeat Config.spriteSize ()
                    |> List.indexedMap
                        (\x () ->
                            case sprite |> Dict.get ( x, y ) of
                                Just False ->
                                    Black

                                Nothing ->
                                    None

                                Just True ->
                                    if Set.member ( x, y ) set then
                                        Primary

                                    else
                                        Secondary
                        )
            )
