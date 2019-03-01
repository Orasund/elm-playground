module LittleWorldPuzzler.Data.Board exposing
    ( Board
    , columns
    , decoder
    , encode
    , place
    , rows
    )

import Grid.Bordered as Grid exposing (Grid)
import Grid.Position exposing (Position)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))


columns : Int
columns =
    4


rows : Int
rows =
    columns


type alias Board =
    Grid CellType


place : Position -> CellType -> Board -> Board
place position cellType =
    Grid.ignoringErrors <|
        Grid.insert position cellType



{------------------------
   Decoder
------------------------}


tupleDecoder : Decoder ( Position, CellType )
tupleDecoder =
    D.map3
        (\x y value ->
            ( ( x, y ), value )
        )
        (D.field "x" D.int)
        (D.field "y" D.int)
        (D.field "value" CellType.decoder)


decoder : Decoder Board
decoder =
    D.map
        (Grid.fromList
            { rows = rows
            , columns = columns
            }
        )
        (D.list tupleDecoder)



{------------------------
   Encoder
------------------------}


tupleEncoder : ( Position, CellType ) -> Value
tupleEncoder ( ( x, y ), cellType ) =
    E.object
        [ ( "x", E.int x )
        , ( "y", E.int y )
        , ( "value", CellType.encode cellType )
        ]


encode : Board -> Value
encode =
    Grid.toList
        >> E.list
            tupleEncoder
