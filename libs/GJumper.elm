module GJumper exposing (Game, GameData,define)

import GJumper.Gui as Gui exposing (Gui)
import Grid.Direction exposing (Direction(..))
import PixelEngine exposing (Area, Background, Input(..), PixelEngine)
import PixelEngine.Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile, Tileset)
import Random exposing (Generator, Seed)
import Grid exposing (Grid)
import Grid.Position as Position exposing (Position)

type alias GameData square data =
    { data : data
    , grid : Grid square
    , player : Position
    }


type Model square model
    = Loading
    | Running 
        { gameData : (GameData square model)
        , seed : Seed
        }


type Msg
    = GotSeed Seed
    | Move Direction


type alias Game square model =
    PixelEngine () (Model square model) Msg


init : () -> ( Model square model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )


update : 
    { initfun: Generator (GameData square model)
    , isValid : (Position -> (GameData square model) -> Bool)
    , tick: (GameData square model -> Generator (Maybe (GameData square model)))
    } -> Msg -> Model square model -> ( Model square model, Cmd Msg )
update {initfun,isValid,tick} msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            seed
                |> Random.step initfun
                |> (\( {data,grid,player}, s ) ->
                        ( Running
                            { seed = s
                            , gameData =
                                { grid = grid
                                , player = player
                                , data = data
                                }
                            }
                        , Cmd.none
                        )
                   )

        ( Move dir, Running {gameData,seed} ) ->
            let
                {player} = gameData

                newPos : Position
                newPos = player |> Position.move 1 dir
            in
            if gameData |> isValid newPos then
                ( model, Cmd.none )
            else
            case seed |> Random.step (tick {gameData| player = newPos}) of
                ( Nothing, _ ) ->
                    init ()

                ( Just m2, s ) ->
                    ( Running
                        { gameData = m2
                        , seed = s
                        }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Move Up

        InputDown ->
            Just <| Move Down

        InputLeft ->
            Just <| Move Left

        InputRight ->
            Just <| Move Right

        _ ->
            Nothing



areas : (GameData square model -> Gui) -> Model square model -> Int -> List (Area msg)
areas fun model imgSize =
    case model of
        Loading ->
            []

        Running {gameData} ->
            gameData
                |> fun 
                |> Gui.toAreas imgSize
                |> List.map (PixelEngine.mapArea never)


view :
    (GameData square model -> Gui)
    -> String
    -> Int
    -> Model square model
    -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
view fun title imgSize model =
    { title = title
    , options = Just Options.default
    , body = areas fun model imgSize
    }


define :
    { init : Generator (GameData square model)
    , isValid : Position -> GameData square model -> Bool
    , tick : GameData square model -> Generator (Maybe (GameData square model))
    , view : GameData square model -> Gui
    , title : String
    , imgSize : Int
    }
    -> Game square model
define config =
    PixelEngine.game
        { init = init
        , update = update
            { initfun = config.init
            , isValid = config.isValid
            , tick = config.tick
            }
        , subscriptions = always Sub.none
        , view = view config.view config.title config.imgSize
        , controls = controls
        , width = toFloat config.imgSize * 16
        }
