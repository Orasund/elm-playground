module LittleWorldPuzzler.Data.Deck exposing
    ( Deck
    , Selected(..)
    , decoder
    , encode
    , first
    , fromList
    , playFirst
    , playSecond
    , played
    , remaining
    , second
    , shuffle
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import List.Zipper as Zipper exposing (Zipper(..))
import LittleWorldPuzzler.Data.CellType as CellType exposing (CellType(..))
import Random exposing (Generator)
import Random.List as RandomList


type Selected
    = First
    | Second


type alias Deck =
    Zipper CellType


fromList : List CellType -> Deck
fromList =
    Zipper.fromList
        >> Zipper.withDefault Wood


remaining : Deck -> List CellType
remaining =
    Zipper.after


played : Deck -> List CellType
played =
    Zipper.before


first : Deck -> CellType
first =
    Zipper.current


second : Deck -> Maybe CellType
second =
    Zipper.after
        >> List.head


playFirst : Deck -> Generator Deck
playFirst deck =
    case deck |> Zipper.next of
        Just newDeck ->
            Random.constant newDeck

        Nothing ->
            deck |> shuffle


playSecond : Deck -> Deck
playSecond deck =
    let
        a =
            deck |> first
    in
    case deck |> second of
        Just b ->
            deck
                |> Zipper.mapBefore ((::) b)
                |> Zipper.mapAfter (List.tail >> Maybe.withDefault [])

        Nothing ->
            deck


shuffle : Deck -> Generator Deck
shuffle =
    Zipper.toList
        >> RandomList.shuffle
        >> Random.map fromList



{------------------------
   Decoder
------------------------}


decoder : Decoder Deck
decoder =
    D.map3
        (\remainingD firstD playedD ->
            Zipper.singleton firstD
                |> Zipper.mapBefore (always playedD)
                |> Zipper.mapAfter (always remainingD)
        )
        ((D.map (Maybe.withDefault []) << D.maybe << D.field "remaining") <|
            D.list <|
                CellType.decoder
        )
        (D.field "first" <| CellType.decoder)
        ((D.map (Maybe.withDefault []) << D.maybe << D.field "played") <|
            D.list <|
                CellType.decoder
        )



{------------------------
   Encoder
------------------------}


encode : Deck -> Value
encode deck =
    E.object
        [ ( "remaining", E.list CellType.encode <| remaining deck )
        , ( "first", CellType.encode <| first deck )
        , ( "played", E.list CellType.encode <| played deck )
        ]
