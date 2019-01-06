module RuineJump.Main exposing (main)

import Css
import Dict exposing (Dict)
import List.Zipper as Zipper exposing (Zipper)
import PixelEngine exposing (PixelEngine, program)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Tile exposing (Tileset)
import Process
import Random
import RuineJump.Config as Config
import RuineJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuineJump.MapSegment as MapSegment
import RuineJump.Player as Player exposing (FaceingDirection(..), Player, PlayerAction(..))
import Task
import Time


type alias Map =
    Dict ( Int, Int ) MapElement


type alias Model =
    { map : Map
    , lowestY : Int
    , xSlice : Zipper Int
    , player : Player
    , seed : Random.Seed
    }


type Msg
    = Init Int
    | Tick
    | Input Input


getSlice : Int -> Random.Seed -> Map -> ( Zipper Int, Random.Seed )
getSlice lowestY seed map =
    let
        getWeights : List a -> Random.Seed -> ( List Int, Random.Seed )
        getWeights list oldSeed =
            Random.step
                (Random.list
                    (list |> List.length)
                    (Random.int Random.minInt Random.maxInt)
                )
                oldSeed

        orderedSlice =
            map
                |> Dict.filter (\( _, y ) _ -> y == lowestY)
                |> Dict.toList

        ( weights, newSeed ) =
            getWeights orderedSlice seed

        shuffledSlice =
            orderedSlice
                |> List.map2
                    (\s ( ( x, _ ), _ ) -> ( x, s ))
                    weights
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
                |> Zipper.fromList
                |> Zipper.withDefault 0
    in
    ( shuffledSlice, newSeed )


tickTask : Cmd Msg
tickTask =
    let
        delay =
            200
    in
    Task.perform (always Tick)
        (Process.sleep delay
            |> Task.andThen (\_ -> Time.now)
        )


restart : ( Maybe Model, Cmd Msg )
restart =
    ( Nothing
    , Random.generate Init <| Random.int Random.minInt Random.maxInt
    )


init : Int -> ( Maybe Model, Cmd Msg )
init int =
    let
        ( map, seed ) =
            [ MapSegment.floorGenerator 0
            , MapSegment.parkourGenerator 1
            , MapSegment.parkourGenerator 2
            , MapSegment.parkourGenerator 3
            , MapSegment.parkourGenerator 4
            , MapSegment.parkourGenerator 5
            ]
                |> List.foldl
                    (\generator ( oldMap, oldSeed ) ->
                        Random.step generator oldSeed
                            |> (\( newSegment, newSeed ) ->
                                    ( oldMap |> Dict.union newSegment, newSeed )
                               )
                    )
                    ( Dict.empty, Random.initialSeed int )

        lowestY =
            0
    in
    ( Just
        { seed = seed
        , player = { pos = ( Config.width // 2, -7 ), action = Standing, faceing = FaceingLeft }
        , map = map
        , lowestY = lowestY
        , xSlice =
            map
                |> getSlice lowestY seed
                |> Tuple.first
        }
    , tickTask
    )


update : Msg -> Maybe Model -> ( Maybe Model, Cmd Msg )
update msg maybeModel =
    let
        defaultCase =
            ( maybeModel, Cmd.none )
    in
    case maybeModel of
        Nothing ->
            case msg of
                Init int ->
                    init int

                _ ->
                    defaultCase

        Just ({ player, map, lowestY, xSlice, seed } as model) ->
            case msg of
                Init int ->
                    init int

                Tick ->
                    let
                        ( _, playerY ) =
                            player.pos
                    in
                    if lowestY <= playerY then
                        restart

                    else
                        let
                          {newPlayer,nextTick} =
                            Player.update
                                player
                                map
                                (\elem -> case elem of
                                    Nothing ->
                                        False

                                    Just (BlockElement Air _) ->
                                        False

                                    Just _ ->
                                        True
                                )
                        in
                        (Just { model | player = newPlayer }
                        , if nextTick then tickTask else Cmd.none
                        )

                Input input ->
                    let
                        ( ( newSlice, newSeed ), newLowestY ) =
                            case xSlice |> Zipper.next of
                                Just slice ->
                                    ( ( slice, seed ), lowestY )

                                Nothing ->
                                    ( map |> getSlice (lowestY - 1) seed
                                    , lowestY - 1
                                    )

                        newMap =
                            let
                                x : Int
                                x =
                                    xSlice |> Zipper.current
                            in
                            map
                                |> Dict.update
                                    ( x, lowestY )
                                    (Maybe.map <|
                                        \element ->
                                            case element of
                                                PlayerElement _ _ ->
                                                    BlockElement Air 0

                                                BlockElement _ id ->
                                                    BlockElement Air id
                                    )
                                |> (if
                                        lowestY
                                            |> modBy Config.sectionHeight
                                            |> (==) 0
                                    then
                                        Dict.filter (\( _, y ) _ -> y <= lowestY)

                                    else
                                        identity
                                   )

                        newModel : Player -> Maybe Model
                        newModel newPlayer =
                            { model
                                | player = newPlayer
                                , map = newMap
                                , xSlice = newSlice
                                , seed = newSeed
                                , lowestY = newLowestY
                            }
                                |> Just
                    in
                    case input of
                        InputA ->
                            defaultCase

                        InputUp ->
                            ( newModel (player |> Player.jump map)
                            , tickTask
                            )

                        InputLeft ->
                            ( newModel (player |> Player.move FaceingLeft map)
                            , tickTask
                            )

                        InputDown ->
                            defaultCase

                        InputRight ->
                            ( newModel (player |> Player.move FaceingRight map)
                            , tickTask
                            )

                        InputX ->
                            defaultCase

                        InputY ->
                            defaultCase

                        InputB ->
                            defaultCase

                        InputNone ->
                            defaultCase


subscriptions : Maybe Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Maybe Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view maybeModel =
    let
        width : Float
        width =
            toFloat <| 3 * Config.width

        height : Float
        height =
            toFloat <| 3 * Config.width

        options =
            Graphics.options
                { width = width
                , transitionSpeedInSec = 0.5
                }

        rows : Int
        rows =
            Config.width

        tileset : Tileset
        tileset =
            { source = "tileset.png"
            , spriteWidth = 3
            , spriteHeight = 3
            }
    in
    { title = "Ruine Jump"
    , options = options
    , body =
        [ Graphics.tiledArea
            { background = Graphics.colorBackground <| Css.rgb 68 36 52
            , rows = rows
            , tileset = tileset
            }
            (case maybeModel of
                Just { map, player, lowestY } ->
                    let
                        ( _, centerY ) =
                            ( floor (width / 6) - 1
                            , floor (height / 6) - 1
                            )
                    in
                    map
                        |> Dict.insert player.pos (PlayerElement player.action player.faceing)
                        |> Dict.foldl
                            (\pos elem list ->
                                let
                                    ( posX, posY ) =
                                        pos

                                    ( _, playerY ) =
                                        player.pos

                                    lowestYModSection =
                                        (lowestY // Config.sectionHeight)
                                            |> (*) Config.sectionHeight

                                    heightModSection =
                                        ((playerY + centerY + 2) // floor (height / 6))
                                            - 1
                                            |> (*) (floor (height / 6))

                                    ( x, y ) =
                                        ( posX
                                        , if playerY > -1 * centerY - 1 + lowestYModSection then
                                            posY
                                                + floor (height / 3)
                                                - 1
                                                - lowestYModSection

                                          else
                                            posY
                                                + floor (height / 3)
                                                - 1
                                                - heightModSection
                                        )
                                in
                                list
                                    |> (if
                                            (x >= 0)
                                                && (x < floor (width / 3))
                                                && (y >= 0)
                                                && (y < floor (height / 3))
                                        then
                                            List.append (MapElement.toTiles ( x, y ) elem)

                                        else
                                            identity
                                       )
                            )
                            []

                Nothing ->
                    []
            )
        ]
    }


main : PixelEngine {} (Maybe Model) Msg
main =
    program
        { init = always restart
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input
        }
