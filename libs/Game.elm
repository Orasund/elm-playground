module Game exposing (Game, main)

import Action
import PixelEngine exposing (Area, Input(..), PixelEngine, game)
import PixelEngine.Options as Options exposing (Options)
import Random exposing (Seed)


type Model model
    = Loading
    | Running { seed : Seed, game : model }


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


update : (Seed -> ( model, Cmd msg )) -> (msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update initfun fun msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            initfun seed
                |> Tuple.mapBoth (\m -> Running {seed=seed,game=m}) (Cmd.map Specific)

        ( Specific runningMsg, Running runningModel ) ->
            fun runningMsg runningModel
                |> Action.config
                |> Action.withUpdate Running Specific
                |> Action.withExit (init ())
                |> Action.apply

        _ ->
            ( model, Cmd.none )


controls : (Input -> Maybe msg) -> Input -> Maybe (Msg msg)
controls fun input =
    input
        |> fun
        |> Maybe.map Specific


subscriptions : (model -> Sub msg) -> Model msg -> Sub (Msg msg)
subscriptions fun model =
    case model of
        Running { game } ->
            game
                |> fun
                |> Sub.map Specific

        Loading ->
            Sub.none


areas : (model -> List (Area msg)) -> Model msg -> List (Area (Msg msg))
areas fun model =
    case model of
        Loading ->
            []

        Running runningModel ->
            fun runningModel
                |> List.map (PixelEngine.mapArea Specific)


view :
    (model -> List (Area msg))
    -> Model msg
    -> { title : String, options : Maybe (Options (Msg msg)), body : List (Area (Msg msg)) }
view fun model =
    { title = "One Switch"
    , options = Just Options.default
    , body = areas fun model
    }


main :
    { init : Seed -> ( model, Cmd msg )
    , controls : Input -> Maybe msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> List (Area msg)
    , subscripions : model -> Sub msg
    , width : Int
    }
    -> Game model msg
main config =
    game
        { init = init
        , update = update config.init config.update
        , subscriptions = subscriptions
        , view = view config.view
        , controls = controls config.controls
        , width = config.width
        }
