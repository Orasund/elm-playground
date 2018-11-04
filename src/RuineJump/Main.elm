module RuineJump.Main exposing (main)

import Browser exposing (Document)
import Css
import Dict exposing (Dict)
import DigDigBoom.Component.Map as Map
import Html.Styled exposing (Html)
import Html.Styled.Events as Events
import PixelEngine exposing (PixelEngine, program)
import PixelEngine.Controls as Controls exposing (Input(..))
import PixelEngine.Graphics as Graphics exposing (Area, Options)
import PixelEngine.Graphics.Image as Image exposing (image)
import PixelEngine.Graphics.Tile as Tile exposing (Tile, Tileset)
import Process
import Random
import RuineJump.MapElement as MapElement exposing (MapElement(..))
import RuineJump.MapSegment as MapSegment
import RuineJump.Player as Player exposing (FaceingDirection(..), Player, PlayerAction(..))
import RuineJump.Tileset as Tileset
import Task
import Time


type alias Map =
    Dict ( Int, Int ) MapElement


type alias Model =
    { map : Map
    , player : Player
    , seed : Random.Seed
    }


type Msg
    = Init Int
    | Tick
    | Input Input
    | None


tickTask : Cmd Msg
tickTask =
    let
        delay =
            50
    in
    Task.perform (always Tick)
        (Process.sleep delay
            |> Task.andThen (\_ -> Time.now)
        )


init : Int -> ( Maybe Model, Cmd Msg )
init int =
    let
        ( segment, seed ) =
            Random.step MapSegment.floorGenerator <| Random.initialSeed int
    in
    ( Just
        { seed = seed
        , player = { pos = ( 10, -15 ), action = Standing, faceing = FaceingLeft }
        , map = segment
        }
    , tickTask
    )


updatePlayer : Player -> Map -> ( Player, Cmd Msg )
updatePlayer ({ pos, action } as player) map =
    let
        ( x, y ) =
            pos

        defaultCase : ( Player, Cmd Msg )
        defaultCase =
            ( { player | action = Standing }, Cmd.none )
    in
    case map |> Dict.get ( x, y + 2 ) of
        Nothing ->
            case map |> Dict.get ( x + 1, y + 2 ) of
                Nothing ->
                    ( player |> Player.fall, tickTask )

                Just _ ->
                    defaultCase

        Just _ ->
            defaultCase


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

        Just ({ player, map } as model) ->
            case msg of
                Init int ->
                    init int

                Tick ->
                    updatePlayer player map
                        |> Tuple.mapFirst
                            (\newPlayer -> Just { model | player = newPlayer })

                Input input ->
                    case input of
                        InputA ->
                            defaultCase

                        InputUp ->
                            defaultCase

                        InputLeft ->
                            ( Just { model | player = player |> Player.move FaceingLeft map }, tickTask )

                        InputDown ->
                            defaultCase

                        InputRight ->
                            ( Just { model | player = player |> Player.move FaceingRight map }, tickTask )

                        InputX ->
                            defaultCase

                        InputY ->
                            defaultCase

                        InputB ->
                            defaultCase

                        InputNone ->
                            defaultCase

                None ->
                    defaultCase


subscriptions : Maybe Model -> Sub Msg
subscriptions maybeModel =
    Sub.none


view : Maybe Model -> { title : String, options : Options Msg, body : List (Area Msg) }
view maybeModel =
    let
        width : Float
        width =
            60

        height : Float
        height =
            60

        options =
            Graphics.options
                { width = width
                , transitionSpeedInSec = 0.5
                }

        rows : Int
        rows =
            20

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
            { background = Graphics.colorBackground <| Css.rgb 255 255 255
            , rows = rows
            , tileset = tileset
            }
            (case maybeModel of
                Just { map, player } ->
                    let
                        (( centerX, centerY ) as center) =
                            ( floor (width / 6) - 1, floor (height / 6) - 1 )
                    in
                    map
                        |> Dict.insert player.pos (PlayerElement player.action player.faceing)
                        |> Dict.foldl
                            (\pos elem list ->
                                let
                                    ( posX, posY ) =
                                        pos

                                    ( playerX, playerY ) =
                                        player.pos

                                    ( x, y ) =
                                        ( posX, posY - playerY + centerY )
                                in
                                list
                                    |> (if
                                            x
                                                >= 0
                                                && (x < floor (width / 3))
                                                && y
                                                >= 0
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
        { init =
            \_ ->
                ( Nothing
                , Random.generate Init <| Random.int Random.minInt Random.maxInt
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        , controls = Input
        }
