module AsteroidMiner.Main exposing (main)

import Action exposing (Action)
import AsteroidMiner.Data exposing (fps, framesPerComet, size)
import AsteroidMiner.Data.Comet as Comet exposing (Comet)
import AsteroidMiner.Tileset as Tileset exposing (tileset)
import Browser
import Color
import Dict exposing (Dict)
import Grid.Bordered as Grid exposing (Grid)
import Grid.Position as Position exposing (Coord, Position)
import Html.Attributes as Attributes
import Location exposing (Angle(..), Location, Vector)
import PixelEngine
    exposing
        ( Area
        , Background
        , Input(..)
        , PixelEngine
        , gameWithNoControls
        )
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import Random exposing (Generator, Seed)
import Time



----------------------
-- Model
----------------------


type Block
    = Ground
    | Mountain
    | Ore_Ground


type alias State =
    { comet : Comet
    , map : Grid Block
    }


type Model
    = Loading
    | Game ( State, Seed )


type GameMsg
    = TimePast


type LoadingMsg
    = GotSeed Seed


type Msg
    = GameSpecific GameMsg
    | LoadingSpecific LoadingMsg


type alias LoadingAction =
    Action Never Never Seed Never


type alias GameAction =
    Action (Generator State) Never Never ()



----------------------
-- Init
----------------------


initGame : Seed -> ( Model, Cmd Msg )
initGame oldSeed =
    let
        center : Int
        center =
            size // 2

        ( angle, seed ) =
            oldSeed
                |> Random.step (Random.float 0 (2 * pi) |> Random.map Angle)

        comet : Comet
        comet =
            Comet.new angle

        map : Grid Block
        map =
            Grid.fill
                (\( x, y ) ->
                    if (x - center) ^ 2 + (y - center) ^ 2 <= 4 ^ 2 then
                        Just <|
                            if abs (x + y - center * 2) < 3 then
                                Mountain

                            else
                                Ground

                    else
                        Nothing
                )
                { rows = size
                , columns = size
                }

        state : State
        state =
            { comet = comet
            , map = map
            }
    in
    ( Game
        ( state, seed )
    , Cmd.none
    )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.independentSeed |> Random.generate (LoadingSpecific << GotSeed)
    )



----------------------
-- Update
----------------------


updateLoading : LoadingMsg -> LoadingAction
updateLoading msg =
    case msg of
        GotSeed seed ->
            Action.transitioning seed


updateGame : GameMsg -> State -> GameAction
updateGame msg ({ comet, map } as state) =
    let
        defaultCase : GameAction
        defaultCase =
            Action.updating ( Random.constant state, Cmd.none )
    in
    case msg of
        TimePast ->
            Action.updating
                ( Random.constant
                    { state
                        | comet =
                            comet
                                |> Comet.update
                    }
                , Cmd.none
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingSpecific loadingMsg, Loading ) ->
            updateLoading loadingMsg
                |> Action.config
                |> Action.withTransition initGame
                |> Action.apply

        ( GameSpecific gameMsg, Game ( state, seed ) ) ->
            updateGame gameMsg state
                |> Action.config
                |> Action.withExit (init ())
                |> Action.withUpdate
                    (\generator -> Game <| Random.step generator seed)
                    never
                |> Action.apply

        _ ->
            ( model, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        second : Float
        second =
            1000
    in
    Time.every (second / fps) (always <| GameSpecific <| TimePast)



----------------------
-- View
----------------------


viewBlock : Block -> Tile msg
viewBlock block =
    case block of
        Ground ->
            Tileset.ground

        Mountain ->
            Tileset.mountain

        Ore_Ground ->
            Tileset.ore_ground


viewMap : Grid Block -> List ( Position, Tile msg )
viewMap =
    Grid.map (\_ -> Maybe.map viewBlock)
        >> Grid.toList


viewComet : Comet -> ( Position, Tile msg )
viewComet comet =
    ( Debug.log "position" <| Comet.position comet, Tileset.comet )


areas : State -> List (Area Msg)
areas { comet, map } =
    [ PixelEngine.tiledArea
        { rows = size
        , tileset = tileset
        , background =
            PixelEngine.imageBackground
                { source = "background.png"
                , width = 8
                , height = 8
                }
        }
      <|
        List.concat
            [ viewMap map
            , [ viewComet comet ]
            ]
    ]


view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "Tic Tac Toe"
    , options =
        Just
            (Options.default
                |> Options.withMovementSpeed (1 / fps)
            )
    , body =
        case model of
            Loading ->
                []

            Game ( state, _ ) ->
                areas state
    }


height : Float
height =
    toFloat <| (size * 8)


main : PixelEngine () Model Msg
main =
    gameWithNoControls
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , width = height
        }
