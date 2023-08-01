module Connection exposing (..)

import Cell exposing (Cell(..), Connection)
import Dict
import RelativePos exposing (RelativePos)


connectionSendsTo : RelativePos -> Connection -> Bool
connectionSendsTo to connection =
    connection.sendsTo
        |> Dict.keys
        |> List.any ((==) to)
