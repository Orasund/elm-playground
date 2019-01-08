module RuineJump.Main exposing (main)

import Css
import Dict exposing (Dict)
import List.Zipper as Zipper exposing (Zipper)
import PixelEngine exposing (PixelEngine, program)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)
import Process
import Random exposing (Generator)
import RuineJump.Config as Config
import RuineJump.Map as Map exposing (Map)
import RuineJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuineJump.MapSegment as MapSegment
import RuineJump.MapSlice as MapSlice
import RuineJump.Player as Player exposing (FaceingDirection(..), Player, PlayerAction(..))
import Task
import Time


type alias Model =
    { map : Map
    , lowestY : Int
    , currentY : Int
    , xSlice : Zipper Int
    , player : Player
    , seed : Random.Seed
    }


type Msg
    = Init Int
    | Tick
    | Input Input


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
        ( { map, xSlice }, seed ) =
            Random.initialSeed int
                |> Random.step
                    (MapSegment.concat
                        [ MapSegment.floorGenerator 0
                        , MapSegment.parkourGenerator 1
                        , MapSegment.parkourGenerator 2
                        , MapSegment.parkourGenerator 3
                        , MapSegment.parkourGenerator 4
                        , MapSegment.parkourGenerator 5
                        ]
                        |> Random.andThen
                            (\newMap ->
                                (newMap |> MapSlice.generator lowestY)
                                    |> Random.map
                                        (\newXSlice ->
                                            { map = newMap
                                            , xSlice = newXSlice
                                            }
                                        )
                            )
                    )

        (( _, y ) as pos) =
            ( Config.width // 2, -7 )

        lowestY : Int
        lowestY =
            0

        player : Player
        player =
            { pos = pos, action = Standing, faceing = FaceingLeft }
    in
    ( Just
        { seed = seed
        , player = player
        , map = map
        , lowestY = lowestY
        , currentY = y
        , xSlice = xSlice
        }
    , tickTask
    )


applyAction : (Player -> Player) -> Model -> Model
applyAction action ({ player, map, lowestY, xSlice, seed } as model) =
    let
        x : Int
        x =
            xSlice |> Zipper.current

        ( { newSlice, newLowestY }, newSeed ) =
            case xSlice |> Zipper.next of
                Just slice ->
                    ( { newSlice = slice, newLowestY = lowestY }, seed )

                Nothing ->
                    seed
                        |> Random.step
                            (MapSlice.generator (lowestY - 1) map
                                |> Random.map
                                    (\slice ->
                                        { newSlice = slice
                                        , newLowestY = lowestY - 1
                                        }
                                    )
                            )
    in
    { model
        | player = player |> action
        , map = map |> Map.remove ( x, lowestY )
        , xSlice = newSlice
        , seed = newSeed
        , lowestY = newLowestY
    }


onInput : Input -> Model -> Maybe ( Model, Cmd Msg )
onInput input ({ map } as model) =
    (case input of
        InputUp ->
            Just <| Player.jump map

        InputLeft ->
            Just <| Player.move FaceingLeft map

        InputDown ->
            Nothing

        InputRight ->
            Just <| Player.move FaceingRight map

        _ ->
            Nothing
    )
        |> Maybe.map
            (\action ->
                model
                    |> applyAction action
                    |> (\newModel ->
                            ( newModel
                            , tickTask
                            )
                       )
            )


onTick : Model -> ( Maybe Model, Cmd Msg )
onTick ({ map, player, lowestY } as model) =
    let
        ( _, playerY ) =
            player.pos
    in
    if lowestY <= playerY then
        restart

    else
        let
            { newPlayer, nextTick } =
                Player.update
                    player
                    map
                    (\elem ->
                        case elem of
                            Nothing ->
                                False

                            Just (BlockElement Air _) ->
                                False

                            Just _ ->
                                True
                    )

            ( _, y ) =
                newPlayer.pos
        in
        if nextTick then
            ( Just { model | player = newPlayer }
            , tickTask
            )

        else
            ( Just
                { model
                    | player = newPlayer
                    , currentY = y
                }
            , Cmd.none
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

        Just model ->
            case msg of
                Init int ->
                    init int

                Tick ->
                    model |> onTick

                Input input ->
                    case model |> onInput input of
                        Nothing ->
                            defaultCase

                        Just ( newModel, cmd ) ->
                            ( Just newModel, cmd )


subscriptions : Maybe Model -> Sub Msg
subscriptions _ =
    Sub.none


getTilesList : { currentY : Int, lowestY : Int } -> Map -> List ( ( Int, Int ), Tile Msg )
getTilesList { currentY, lowestY } =
    Dict.foldl
        (\pos elem list ->
            let
                ( posX, posY ) =
                    pos

                ( _, centerY ) =
                    ( (Config.width // 2) - 1
                    , (Config.width // 2) - 1
                    )

                lowestYModSection =
                    (lowestY // Config.sectionHeight)
                        |> (*) Config.sectionHeight

                heightModSection =
                    ((currentY + centerY + 2) // (Config.width // 2))
                        - 1
                        |> (*) (Config.width // 2)

                ( x, y ) =
                    ( posX
                    , if currentY > -1 * centerY - 1 + lowestYModSection then
                        posY
                            + Config.width
                            - 1
                            - lowestYModSection

                      else
                        posY
                            + Config.width
                            - 1
                            - heightModSection
                    )
            in
            list
                |> (if
                        (x >= 0)
                            && (x < Config.width)
                            && (y >= 0)
                            && (y < Config.width)
                    then
                        List.append <|
                            MapElement.toTiles
                                ( x, y )
                                elem

                    else
                        identity
                   )
        )
        []


view : Maybe Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view maybeModel =
    let
        width : Float
        width =
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
                Just { map, player, currentY, lowestY } ->
                    map
                        |> Dict.insert
                            player.pos
                            (PlayerElement player.action player.faceing)
                        |> getTilesList
                            { currentY = currentY, lowestY = lowestY }

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
