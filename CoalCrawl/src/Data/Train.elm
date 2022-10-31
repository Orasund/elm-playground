module Data.Train exposing (..)

import Data.Item exposing (Item)


type alias Train =
    { pos : ( Int, Int )
    , coal : Int
    , coalNeeded : Int
    , dir : ( Int, Int )
    , moving : Bool
    , tracks : Int
    }


fromPos : ( Int, Int ) -> Train
fromPos pos =
    { pos = pos
    , coal = 0
    , coalNeeded = 4
    , dir = ( 0, -1 )
    , moving = False
    , tracks = 0
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


turnAround : Train -> Train
turnAround train =
    let
        ( x, y ) =
            train.dir
    in
    { train | dir = ( x, -y ) }


addItem : Item -> Train -> Train
addItem item train =
    case item of
        Data.Item.Coal ->
            addCoal train


addCoal : Train -> Train
addCoal train =
    { train | coal = train.coal + 1 }
        |> (\t ->
                if t.coal >= t.coalNeeded || (train.tracks > 0) then
                    { t | moving = True }

                else
                    t
           )


removeCoal : Train -> Maybe Train
removeCoal train =
    if train.coal > 0 then
        { train | coal = train.coal - 1 }
            |> (\t ->
                    if t.coal == 0 then
                        { t | moving = False }

                    else
                        t
               )
            |> Just

    else
        Nothing


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
            , coalNeeded = train.coalNeeded + 2
        }
            |> (\t ->
                    if t.tracks == 0 then
                        t

                    else
                        t
               )
            |> Just

    else
        Nothing
