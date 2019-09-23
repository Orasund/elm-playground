module GJumper exposing (Footer, Game, GameData, Header, InitModel, Status(..), View, define, footer, header, view, withGui)

import GJumper.Core as Core
import Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import PixelEngine exposing (Area, Background, Input(..), PixelEngine)
import PixelEngine.Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile, Tileset)
import Random exposing (Generator, Seed)


type alias GameData square data =
    { data : data
    , grid : Grid square
    , player : Position
    }


type Status
    = Ongoing
    | Won
    | Lost


type alias InitModel square data =
    { data : data
    , player : Position
    , distribution :
        data
        -> ( ( Float, Maybe square ), List ( Float, Maybe square ) )
    , fixed : data -> List ( Int, square )
    , level : data -> List (List (Maybe square))
    , rows : Int
    , columns : Int
    }


type Model square data
    = Loading
    | Running
        { gameData : GameData square data
        , seed : Seed
        , status : Status
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
    { initfun :
        Maybe data
        ->
            Generator
                { data : data
                , player : Position
                , distribution :
                    data
                    -> ( ( Float, Maybe square ), List ( Float, Maybe square ) )
                , fixed : data -> List ( Int, square )
                , level : data -> List (List (Maybe square))
                , rows : Int
                , columns : Int
                }
    , isSolid : square -> Bool
    , tick : GameData square data -> Generator ( GameData square data, Status )
    }
    -> Msg
    -> Model square data
    -> ( Model square data, Cmd Msg )
update { initfun, isSolid, tick } msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            seed
                |> Random.step
                    (initfun Nothing
                        |> Random.andThen
                            (\{ data, player, distribution, fixed, level, rows, columns } ->
                                Core.gridGenerator data
                                    { distribution = distribution
                                    , fixed = fixed
                                    , level = level
                                    , rows = rows
                                    , columns = columns
                                    }
                                    |> Random.map
                                        (\grid ->
                                            { grid = grid |> Grid.remove player
                                            , player = player
                                            , data = data
                                            }
                                        )
                            )
                    )
                |> (\( gameData, s ) ->
                        ( Running { seed = s, gameData = gameData, status = Ongoing }
                        , Cmd.none
                        )
                   )

        ( Move dir, Running { status, gameData, seed } ) ->
            let
                newPos : Position
                newPos =
                    gameData.player
                        |> Position.move 1 dir
                        |> (\( x, y ) -> ( x |> modBy 16, y |> modBy 16 ))
            in
            if status /= Ongoing then
                seed
                    |> Random.step
                        (initfun (Just gameData.data)
                            |> Random.andThen
                                (\{ data, player, distribution, fixed, level, rows, columns } ->
                                    Core.gridGenerator data
                                        { distribution = distribution
                                        , fixed = fixed
                                        , level = level
                                        , rows = rows
                                        , columns = columns
                                        }
                                        |> Random.map
                                            (\grid ->
                                                { grid = grid |> Grid.remove player
                                                , player = player
                                                , data = data
                                                }
                                            )
                                )
                        )
                    |> (\( gD, s ) ->
                            ( Running { seed = s, gameData = gD, status = Ongoing }
                            , Cmd.none
                            )
                       )

            else if
                gameData.grid
                    |> Grid.get newPos
                    |> Maybe.map isSolid
                    |> Maybe.withDefault False
            then
                ( model, Cmd.none )

            else
                seed
                    |> Random.step (tick { gameData | player = newPos })
                    |> (\( ( gd, st ), s ) ->
                            ( Running
                                { gameData = gd
                                , seed = s
                                , status = st
                                }
                            , Cmd.none
                            )
                       )

        ( Reset, _ ) ->
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

                ( playerX, playerY ) =
                    player

                { columns, rows } =
                    grid |> Grid.dimensions
            in
            grid
                |> Grid.toList
                |> List.filterMap
                    (\( ( x, y ), a ) ->
                        let
                            newX : Int
                            newX =
                                x - playerX + 7 |> modBy columns

                            newY : Int
                            newY =
                                y - playerY + 7 |> modBy rows
                        in
                        if newX < 16 && newY < 16 then
                            Just
                                ( ( newX
                                  , newY
                                  )
                                , a
                                )

                        else
                            Nothing
                    )
                |> List.map
                    (\( pos, square ) ->
                        ( pos, square |> render.square )
                    )
                |> (::) ( (7,7), render.player )
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
    { init :
        Maybe data
        ->
            Generator
                { data : data
                , player : Position
                , distribution :
                    data
                    -> ( ( Float, Maybe square ), List ( Float, Maybe square ) )
                , fixed : data -> List ( Int, square )
                , level : data -> List (List (Maybe square))
                , rows : Int
                , columns : Int
                }
    , isSolid : square -> Bool
    , tick : GameData square data -> Generator ( GameData square data, Status )
    , view : data -> View square
    , title : String
    , imgSize : Int
    }
    -> Game square data
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
