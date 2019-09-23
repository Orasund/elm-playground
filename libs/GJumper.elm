module GJumper exposing (Footer, Game, GameData, Header, View, define, footer, header, view, withGui)

import GJumper.Core as Core
import Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import PixelEngine exposing (Area, Background, Input(..), PixelEngine)
import PixelEngine.Image as Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile, Tileset)
import Random exposing (Generator, Seed)


type alias GameData square data =
    { data : data
    , grid : Grid square
    , player : Position
    }


type Model square model
    = Loading
    | Running
        { gameData : GameData square model
        , seed : Seed
        }


type Msg
    = GotSeed Seed
    | Move Direction
    | Reset


type alias Game square model =
    PixelEngine () (Model square model) Msg


init : () -> ( Model square model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )

update :
    { initfun : Generator (GameData square model)
    , isSolid : square -> Bool
    , tick : GameData square model -> Generator (Maybe (GameData square model))
    }
    -> Msg
    -> Model square model
    -> ( Model square model, Cmd Msg )
update { initfun, isSolid, tick } msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            seed
                |> Random.step initfun
                |> (\( { data, grid, player }, s ) ->
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

        ( Move dir, Running { gameData, seed } ) ->
            let
                { player,grid } =
                    gameData

                newPos : Position
                newPos =
                    player
                    |> Position.move 1 dir
                    |> \(x,y) -> (x |> modBy 16,y |> modBy 16)
            in
            if grid |> Grid.get newPos |> Maybe.map isSolid |> Maybe.withDefault False then
                ( model, Cmd.none )

            else
                case seed |> Random.step (tick { gameData | player = newPos }) of
                    ( Nothing, _ ) ->
                        init ()

                    ( Just m2, s ) ->
                        ( Running
                            { gameData = m2
                            , seed = s
                            }
                        , Cmd.none
                        )

        (Reset,_) ->
            init ()

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
        
        InputB ->
            Just <| Reset

        _ ->
            Nothing


areas : (data -> View square) -> Model square data -> Int -> List (Area msg)
areas fun model imgSize =
    case model of
        Loading ->
            []

        Running { gameData } ->
            let
                { data, player, grid } =
                    gameData

                (Core.View render) =
                    fun data

                ( h, f ) =
                    render.gui
            in
            grid
                |> Grid.toList
                |> List.map
                    (\( pos, square ) ->
                        ( pos, square |> render.square )
                    )
                |> (::) ( player, render.player )
                |> Core.create render.background render.tileset
                |> Core.withHeader h
                |> Core.withFooter f
                |> Core.toAreas imgSize
                |> List.map (PixelEngine.mapArea never)


viewFun :
    (model -> View square)
    -> String
    -> Int
    -> Model square model
    -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
viewFun fun title imgSize model =
    { title = title
    , options = Just Options.default
    , body = areas fun model imgSize
    }


define :
    { init : Generator (GameData square model)
    , isSolid : square -> Bool
    , tick : GameData square model -> Generator (Maybe (GameData square model))
    , view : model -> View square
    , title : String
    , imgSize : Int
    }
    -> Game square model
define config =
    PixelEngine.game
        { init = init
        , update =
            update
                { initfun = config.init
                , isSolid = config.isSolid
                , tick = config.tick
                }
        , subscriptions = always Sub.none
        , view = viewFun config.view config.title config.imgSize
        , controls = controls
        , width = toFloat config.imgSize * 16
        }


gridGenerator : a -> 
    { distribution : a -> ((Float,Maybe square),List (Float,Maybe square))
    , fixed : a -> List (Int, square)
    , level : a -> List (List (Maybe square))
    } -> Generator (Grid square)
gridGenerator =
    Core.gridGenerator

------------------------------------------------------
-- View
------------------------------------------------------


type alias View square =
    Core.View square


type alias Header =
    Core.Header


type alias Footer =
    Core.Footer


header : List ( Float, Image Never ) -> Header
header =
    Core.Header


footer : List ( Float, Image Never ) -> List ( Float, Image Never ) -> List ( Float, Image Never ) -> Footer
footer l1 l2 l3 =
    let
        mapList : Float -> List ( Float, Image Never ) -> List ( ( Float, Float ), Image Never )
        mapList y =
            List.map (\( x, img ) -> ( ( x, y ), img ))
    in
    [ l1 |> mapList 0
    , l2 |> mapList 1
    , l3 |> mapList 2
    ]
        |> List.concat
        |> Core.Footer


view : { player : Tile Never, square : square -> Tile Never } -> Tileset -> Background -> View square
view { player, square } tileset background =
    Core.View
        { gui = ( Core.Header [], Core.Footer [] )
        , player = player
        , square = square
        , tileset = tileset
        , background =
            { grid = background
            , gui = background
            }
        }


withGui : Header -> Footer -> Background -> View square -> View square
withGui h f b (Core.View ({ background } as v)) =
    Core.View
        { v
            | gui = ( h, f )
            , background =
                { background
                    | gui = b
                }
        }
