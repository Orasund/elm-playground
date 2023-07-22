module Grid exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2, Connection1, Connection2)
import Dict exposing (Dict)
import Dir exposing (Dir)
import Level exposing (Module)


type Grid
    = Stage1 (Dict ( Int, Int ) Cell1)
    | Stage2 (Dict ( Int, Int ) Cell2)


toDict : Grid -> Dict ( Int, Int ) (Cell ())
toDict grid =
    let
        fun : Dict ( Int, Int ) (Cell a) -> Dict ( Int, Int ) (Cell ())
        fun =
            Dict.map (\_ -> Cell.map (\_ -> ()))
    in
    case grid of
        Stage1 dict ->
            fun dict

        Stage2 dict ->
            fun dict


getEmoji : ( Int, Int ) -> Grid -> Maybe String
getEmoji pos grid =
    case grid of
        Stage1 dict ->
            Dict.get pos dict
                |> Maybe.map Cell.cell1ToEmoji

        Stage2 dict ->
            Dict.get pos dict
                |> Maybe.map Cell.cell2ToEmoji


update : Grid -> ( Grid, Bool )
update grid =
    case grid of
        Stage1 dict ->
            dict
                |> Dict.map
                    (\pos cell ->
                        tick ( pos, cell ) dict
                    )
                |> (\d -> ( Stage1 d, Dict.toList d /= Dict.toList dict ))

        Stage2 dict ->
            ( dict |> Stage2, False )


connection2Tick :
    { pos : ( Int, Int )
    , moduleId : Int
    , module_ : Module
    }
    -> Dict ( Int, Int ) Cell2
    -> Connection2
connection2Tick args dict =
    args.module_
        |> Dict.filter
            (\_ { from } ->
                sendsEnergy Cell.cell2sendsEnergyTo
                    { from = Dir.add from args.pos, pos = args.pos }
                    dict
            )
        |> Dict.keys
        |> List.map (\dir -> Dir.add dir args.pos)
        |> (\list -> { moduleId = args.moduleId, activePos = list })


connection1Tick : ( Int, Int ) -> List Dir -> Dict ( Int, Int ) Cell1 -> Connection1
connection1Tick pos neighborsDir dict =
    let
        ( x, y ) =
            pos
    in
    case neighborsDir of
        [ dir1, dir2 ] ->
            if
                sendsEnergy Cell.cell1sendsEnergyTo
                    { from = Dir.add dir1 pos
                    , pos = pos
                    }
                    dict
            then
                [ Dir.add dir2 pos ]

            else if
                sendsEnergy Cell.cell1sendsEnergyTo
                    { from = Dir.add dir2 pos
                    , pos = pos
                    }
                    dict
            then
                [ Dir.add dir1 pos ]

            else
                []

        _ ->
            neighboringDir
                |> List.filter
                    (\fromDir ->
                        sendsEnergy Cell.cell1sendsEnergyTo
                            { from = Dir.add fromDir pos
                            , pos = pos
                            }
                            dict
                    )
                |> List.map
                    (\dir ->
                        Dir.add
                            (dir |> Dir.reverse)
                            ( x, y )
                    )


tick : ( ( Int, Int ), Cell1 ) -> Dict ( Int, Int ) Cell1 -> Cell1
tick ( pos, cell ) dict =
    let
        neighborsDir =
            neighboringDir
                |> List.filterMap
                    (\dir ->
                        case Dict.get (Dir.add dir pos) dict of
                            Just (Glass _) ->
                                Just dir

                            Just Laser ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )
    in
    case cell of
        Glass _ ->
            dict
                |> connection1Tick pos neighborsDir
                |> Glass

        Target _ ->
            neighboringDir
                |> List.any
                    (\fromDir ->
                        sendsEnergy Cell.cell1sendsEnergyTo
                            { from = Dir.add fromDir pos
                            , pos = pos
                            }
                            dict
                    )
                |> Target

        _ ->
            cell


sendsEnergy : (( Int, Int ) -> a -> Bool) -> { from : ( Int, Int ), pos : ( Int, Int ) } -> Dict ( Int, Int ) (Cell a) -> Bool
sendsEnergy fun args dict =
    case dict |> Dict.get args.from of
        Just (Glass to) ->
            fun args.pos to

        Just Laser ->
            True

        _ ->
            False


neighboringDir : List Dir
neighboringDir =
    [ ( -1, 0 )
    , ( 1, 0 )
    , ( 0, -1 )
    , ( 0, 1 )
    ]
        |> List.map Dir.new
