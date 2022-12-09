module Data.Train exposing (..)

import AnyBag exposing (AnyBag)
import Config
import Data.Item exposing (Item)
import Data.Storage exposing (Storage)


type alias Train =
    { pos : ( Int, Int )
    , dir : ( Int, Int )
    , moving : Bool
    , tracks : Int
    , items : AnyBag String Item
    }


fromPos : ( Int, Int ) -> Train
fromPos pos =
    { pos = pos
    , dir = ( 0, -1 )
    , moving = False
    , tracks = 0
    , items = AnyBag.empty Data.Item.toString
    }


forwardPos : Train -> ( Int, Int )
forwardPos train =
    let
        ( dirX, dirY ) =
            train.dir

        ( x, y ) =
            train.pos
    in
    ( x + dirX, y + dirY )


move : Train -> Train
move train =
    { train | pos = forwardPos train }


turnDownwards : Train -> Train
turnDownwards train =
    { train | dir = ( 0, 1 ) }


turnUpwards : Train -> Train
turnUpwards train =
    { train | dir = ( 0, -1 ) }


coalNeeded : Train -> Int
coalNeeded train =
    let
        ( _, y ) =
            train.pos
    in
    y * 2


updateStorage : (Storage -> ( Storage, a )) -> Train -> ( Train, a )
updateStorage fun train =
    Data.Storage.empty Config.trainLoadSize
        |> fun
        |> Tuple.mapFirst
            (\storage ->
                addAll storage.items train
            )


addAll : List Item -> Train -> Train
addAll list train =
    { train
        | items =
            train.items
                |> AnyBag.union (AnyBag.fromList Data.Item.toString list)
    }
        |> (\t ->
                if
                    List.member Data.Item.Coal list
                        && (AnyBag.count Data.Item.Coal t.items >= coalNeeded t || (t.tracks > 0))
                then
                    { t | moving = True }

                else
                    t
           )


addItem : Item -> Train -> Train
addItem item =
    addAll [ item ]


removeItem : Int -> Item -> Train -> Maybe Train
removeItem n item train =
    (if AnyBag.count item train.items >= n then
        { train
            | items =
                train.items
                    |> AnyBag.remove n item
        }
            |> Just

     else
        Nothing
    )
        |> Maybe.map
            (\t ->
                if item == Data.Item.Coal && AnyBag.count Data.Item.Coal t.items == 0 then
                    { t | moving = False }

                else
                    t
            )


addTracks : Int -> Train -> Train
addTracks tracks train =
    { train | tracks = train.tracks + tracks }


stop : Train -> Train
stop train =
    { train | moving = False }


removeTrack : Train -> Maybe Train
removeTrack train =
    if train.moving && train.tracks > 0 then
        { train
            | tracks = train.tracks - 1
        }
            |> Just

    else
        Nothing
