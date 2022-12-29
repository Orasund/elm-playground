module Data.Train exposing (..)

import Config
import Data.Item exposing (Item)
import Data.Storage exposing (Storage)
import ListBag


type alias Train =
    { pos : ( Int, Int )
    , lookingUp : Bool
    , moving : Bool
    , tracks : Int
    , items : List ( Item, Int )
    }


fromPos : ( Int, Int ) -> Train
fromPos pos =
    { pos = pos
    , lookingUp = True
    , moving = False
    , tracks = 0
    , items = []
    }


forwardPos : Train -> ( Int, Int )
forwardPos train =
    let
        ( dirX, dirY ) =
            if train.lookingUp then
                ( 0, -1 )

            else
                ( 0, 1 )

        ( x, y ) =
            train.pos
    in
    ( x + dirX, y + dirY )


move : Train -> Train
move train =
    { train | pos = forwardPos train }


turnDownwards : Train -> Train
turnDownwards train =
    { train | lookingUp = False }


turnUpwards : Train -> Train
turnUpwards train =
    { train | lookingUp = True }


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
                |> ListBag.union (ListBag.fromList list)
    }
        |> (\t ->
                if
                    List.member Data.Item.Coal list
                        && (ListBag.count Data.Item.Coal t.items >= coalNeeded t || (t.tracks > 0))
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
    (if ListBag.count item train.items >= n then
        { train
            | items =
                train.items
                    |> ListBag.remove n item
        }
            |> Just

     else
        Nothing
    )
        |> Maybe.map
            (\t ->
                if item == Data.Item.Coal && ListBag.count Data.Item.Coal t.items == 0 then
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
