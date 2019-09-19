module Game exposing (Game, define)

import PixelEngine exposing (Area, Background, Input(..), PixelEngine)
import PixelEngine.Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile,Tileset)
import Random exposing (Generator, Seed)
import Game.Gui as Gui exposing (Gui)

type Model model
    = Loading
    | Running { game : model, seed : Seed }


type Msg msg
    = GotSeed Seed
    | Specific msg


type alias Game model msg =
    PixelEngine () (Model model) (Msg msg)


init : () -> ( Model model, Cmd (Msg msg) )
init _ =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )


update : Generator ( model, Cmd msg ) -> (msg -> model -> Generator (Maybe ( model, Cmd msg ))) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update initfun fun msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            seed
                |> Random.step initfun
                |> (\( ( g, cmd ), s ) ->
                        ( Running { seed = s, game = g }, cmd |> Cmd.map Specific )
                   )

        ( Specific runningMsg, Running runningModel ) ->
            case runningModel.seed |> Random.step (fun runningMsg runningModel.game) of
                ( Nothing, _ ) ->
                    init ()

                ( Just ( game, cmd ), seed ) ->
                    ( Running { game = game, seed = seed }
                    , cmd |> Cmd.map Specific
                    )

        _ ->
            ( model, Cmd.none )


controls : (Input -> Maybe msg) -> Input -> Maybe (Msg msg)
controls fun input =
    input
        |> fun
        |> Maybe.map Specific


subscriptions : (model -> Sub msg) -> Model model -> Sub (Msg msg)
subscriptions fun model =
    case model of
        Running { game } ->
            game
                |> fun
                |> Sub.map Specific

        Loading ->
            Sub.none


areas : (model-> Gui)-> Model model-> Int  -> List (Area (Msg msg))
areas fun model imgSize =
    case model of
        Loading ->
            []

        Running runningModel ->
            fun runningModel.game
            |> Gui.toAreas imgSize
            |> List.map (PixelEngine.mapArea never)


view :
    (model-> Gui)
    -> String
    -> Int
    -> Model model
    -> { title : String, options : Maybe (Options (Msg msg)), body : List (Area (Msg msg)) }
view fun title imgSize model =
    { title = title
    , options = Just Options.default
    , body = areas fun model imgSize
    }


define :
    { init : Generator ( model, Cmd msg )
    , controls : Input -> Maybe msg
    , update : msg -> model -> Generator (Maybe ( model, Cmd msg ))
    , view :model -> Gui
    , title : String
    , subscriptions : model -> Sub msg
    , imgSize : Int
    }
    -> Game model msg
define config =
    PixelEngine.game
        { init = init
        , update = update config.init config.update
        , subscriptions = subscriptions config.subscriptions
        , view = view config.view config.title config.imgSize
        , controls = controls config.controls
        , width = toFloat config.imgSize * 16
        }
