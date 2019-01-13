module RuinJump.Main exposing (main)

import Color
import Dict
import List.Zipper as Zipper exposing (Zipper)
import PixelEngine exposing (PixelEngine, game)
import PixelEngine.Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Tile exposing (Tile, Tileset)
import Process
import Random exposing (Generator, Seed)
import RuinJump.Config as Config
import RuinJump.Map as Map exposing (Map)
import RuinJump.MapElement as MapElement exposing (Block(..), MapElement(..))
import RuinJump.MapSegment as MapSegment
import RuinJump.MapSlice as MapSlice
import RuinJump.Player as Player
    exposing
        ( FaceingDirection(..)
        , Player
        , PlayerAction(..)
        )
import RuinJump.Stage as Stage exposing (Stage)
import Task
import Time


type alias Model =
    { stage : Stage
    , player : Player
    }


type alias State =
    ( Model, Seed )


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


restart : ( Maybe State, Cmd Msg )
restart =
    ( Nothing
    , Random.generate Init <| Random.int Random.minInt Random.maxInt
    )


generateMap : Int -> (Int -> List (Generator Map)) -> Seed -> ( { map : Map, xSlice : Zipper Int }, Seed )
generateMap lowestY segments =
    Random.step
        (MapSegment.concat
            (List.range 0 10
                |> List.map segments
                |> List.concat
                |> List.append [ MapSegment.floorGenerator 0 ]
            )
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


init : Int -> ( Maybe State, Cmd Msg )
init int =
    let
        segments : Int -> List (Generator Map)
        segments i =
            [ MapSegment.parkourGenerator <| i * 5 + 1
            , MapSegment.parkourGenerator <| i * 5 + 2
            , MapSegment.parkourGenerator <| i * 5 + 3
            , MapSegment.parkourGenerator <| i * 5 + 4
            , MapSegment.intersectionGenerator <| i * 5 + 5
            ]

        ( { map, xSlice }, seed ) =
            Random.initialSeed int
                |> generateMap lowestY segments

        (( _, y ) as pos) =
            ( Config.width // 2, -7 )

        lowestY : Int
        lowestY =
            0

        decaySpeed : Int
        decaySpeed =
            1

        player : Player
        player =
            { pos = pos, action = Standing, faceing = FaceingLeft }
    in
    ( Just
        ( { player = player
          , stage =
                { map = map
                , lowestY = lowestY
                , currentY = y
                , xSlice = xSlice
                , decaySpeed = decaySpeed
                }
          }
        , seed
        )
    , tickTask
    )


removeN : Int -> State -> State
removeN decaySpeed ( model, seed ) =
    let
        { stage } =
            model

        ( newStage, newSeed ) =
            ( stage, seed )
                |> Stage.removeN decaySpeed
    in
    ( { model
        | stage = newStage
      }
    , newSeed
    )


applyAction : (Player -> Player) -> Model -> Model
applyAction action ({ player } as model) =
    { model
        | player = player |> action
    }


placeStairs : Model -> Generator Model
placeStairs ({ player, stage } as model) =
    let
        ( x, y ) =
            player.pos

        ( pos1, pos2 ) =
            case player.faceing of
                FaceingLeft ->
                    ( ( x - 2, y )
                    , ( x - 1, y + 1 )
                    )

                FaceingRight ->
                    ( ( x + 3, y )
                    , ( x + 2, y + 1 )
                    )
    in
    Stage.placeStairs pos1 pos2 stage
        |> Random.map
            (\newStage ->
                { model
                    | stage = newStage
                }
            )


onInput : Input -> State -> Maybe ( State, Cmd Msg )
onInput input (( model,_) as state) =
    let
        { stage } =
            model

        { map, decaySpeed } =
            stage
    in
    (case input of
        InputUp ->
            Just
                ((Player.jump map |> applyAction |> Tuple.mapFirst)
                    >> removeN decaySpeed
                )

        InputLeft ->
            Just
                (Player.move FaceingLeft map |> applyAction |> Tuple.mapFirst)

        InputDown ->
            Just (Player.drop map |> applyAction |> Tuple.mapFirst)

        InputRight ->
            Just (Player.move FaceingRight map |> applyAction |> Tuple.mapFirst)

        InputA ->
            Just (\(m,s) -> s |> Random.step (placeStairs m))

        _ ->
            Nothing
    )
        |> Maybe.map
            (\function ->
                state
                    |> function
                    |> (\newState ->
                            ( newState
                            , tickTask
                            )
                       )
            )


onTick : State -> ( Maybe State, Cmd Msg )
onTick (( model, seed ) as state) =
    let
        { player, stage } =
            model

        { map, lowestY } =
            stage

        ( _, playerY ) =
            player.pos
    in
    if lowestY <= playerY then
        restart

    else
        let
            { newPlayer, nextTick } =
                Player.update player map MapElement.isOccupied

            ( _, y ) =
                newPlayer.pos
        in
        if nextTick then
            ( Just ( { model | player = newPlayer }, seed )
            , tickTask
            )

        else
            ( Just
                ( { model
                    | player = newPlayer
                    , stage =
                        { stage
                            | currentY = y
                        }
                  }
                , seed
                )
            , Cmd.none
            )


update : Msg -> Maybe State -> ( Maybe State, Cmd Msg )
update msg maybeState =
    let
        defaultCase =
            ( maybeState, Cmd.none )
    in
    case maybeState of
        Nothing ->
            case msg of
                Init int ->
                    init int

                _ ->
                    defaultCase

        Just (( model, _ ) as state) ->
            case msg of
                Init int ->
                    init int

                Tick ->
                    state |> onTick

                Input input ->
                    case state |> onInput input of
                        Nothing ->
                            defaultCase

                        Just ( newState, cmd ) ->
                            ( Just newState, cmd )


subscriptions : Maybe State -> Sub Msg
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


view : Maybe State -> { title : String, options : Options Msg, body : List (Area Msg) }
view maybeState =
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
            { background = Graphics.colorBackground <| Color.rgb255 68 36 52
            , rows = rows
            , tileset = tileset
            }
            (case maybeState of
                Just ( { player, stage }, _ ) ->
                    let
                        { map, currentY, lowestY } =
                            stage
                    in
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


main : PixelEngine {} (Maybe State) Msg
main =
    game
        { init = always restart
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input
        }
