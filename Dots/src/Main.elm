module Main exposing (..)

import Browser exposing (Document)
import Dict
import Html
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Value exposing (JsonValue(..))
import Random exposing (Generator, Seed)
import Result.Extra
import String exposing (replace)
import Svg
import Svg.Attributes
import Time
import Tracery
import Tracery.Command exposing (Command(..))
import Tracery.Grammar exposing (Grammar)
import Tracery.Syntax exposing (Expression(..))


backgroundColor : String
backgroundColor =
    "#553A41"


color1 : String
color1 =
    "#32908F"


color2 : String
color2 =
    "#26C485"


type alias Model =
    { seed : Seed
    , grammar : Grammar
    , circles : List ( ( Float, Float ), Circle )
    , error : Maybe String
    }


type Msg
    = NewSeed Seed
    | RequestedNextImage
    | Tick


type alias Circle =
    { size : Float, color : String }


generateGrammar : Result Json.Decode.Error Grammar
generateGrammar =
    """
        { "origin":"#square#"
        , "content":
            [ "\\"#color#\\""
            , "\\"#color#\\""
            , "null"
            , "#square#"
            ]
        , "color": ["\\\\"""
        ++ color1
        ++ """","\\\\"""
        ++ color2
        ++ """"]
        , "square": [ "{\\"1\\":#content#,\\"2\\":#content#,\\"3\\":#content#,\\"0\\":#content#}" ]
        }
        """
        |> Tracery.fromJson


toCircles : ( Float, Float ) -> Float -> Grammar -> Result Json.Decode.Error (List ( ( Float, Float ), Circle ))
toCircles pos size grammar =
    grammar
        |> Tracery.toString
            (\{ variable } ->
                case variable of
                    "color" ->
                        "black"

                    _ ->
                        "\"black\""
            )
        |> Json.Decode.decodeString (circleDecoder pos size)


circleList : { pos : ( Float, Float ), size : Float } -> JsonValue -> List ( ( Float, Float ), Circle )
circleList args jsonValue =
    let
        ( x1, y1 ) =
            args.pos
    in
    case jsonValue of
        StringValue string ->
            [ ( args.pos, { size = args.size, color = string } ) ]

        ObjectValue list ->
            let
                dict =
                    list
                        |> Dict.fromList
            in
            [ ( "0", ( 0, 0 ) ), ( "1", ( 1, 0 ) ), ( "2", ( 0, 1 ) ), ( "3", ( 1, 1 ) ) ]
                |> List.concatMap
                    (\( k, ( x2, y2 ) ) ->
                        dict
                            |> Dict.get k
                            |> Maybe.map
                                (circleList
                                    { pos = ( x1 + x2 * args.size / 2, y1 + y2 * args.size / 2 )
                                    , size = args.size / 2
                                    }
                                )
                            |> Maybe.withDefault []
                    )

        _ ->
            []


circleDecoder : ( Float, Float ) -> Float -> Decoder (List ( ( Float, Float ), Circle ))
circleDecoder pos size =
    Json.Value.decoder
        |> Json.Decode.map (circleList { pos = pos, size = size })


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( grammar, error ) =
            case generateGrammar of
                Ok g ->
                    ( g, Nothing )

                Err err ->
                    ( Tracery.Grammar.fromDefinitions Dict.empty, err |> Json.Decode.errorToString |> Just )
    in
    ( { seed = Random.initialSeed 42
      , circles = []
      , grammar = grammar
      , error = error
      }
    , Random.generate NewSeed Random.independentSeed
    )


view : Model -> Document Msg
view model =
    { title = "Dots"
    , body =
        let
            size =
                400

            imageSize =
                500

            result =
                toCircles ( 50, 50 ) size model.grammar
        in
        [ Html.div []
            [ case result of
                Ok list ->
                    list
                        |> List.map
                            (\( ( x, y ), circle ) ->
                                Svg.circle
                                    [ x + circle.size / 2 |> String.fromFloat |> Svg.Attributes.cx
                                    , y + circle.size / 2 |> String.fromFloat |> Svg.Attributes.cy
                                    , circle.size / 2 |> String.fromFloat |> Svg.Attributes.r
                                    , circle.color |> Svg.Attributes.fill
                                    , "drop-shadow(0px 4px 2px rgb(0 0 0 / 0.4))"
                                        |> Svg.Attributes.filter
                                    ]
                                    []
                            )
                        |> (::)
                            (Svg.rect
                                [ Svg.Attributes.x "0"
                                , Svg.Attributes.y "0"
                                , imageSize |> String.fromFloat |> Svg.Attributes.height
                                , imageSize |> String.fromFloat |> Svg.Attributes.width
                                , backgroundColor |> Svg.Attributes.fill
                                ]
                                []
                            )
                        |> Svg.svg
                            [ imageSize |> String.fromFloat |> Svg.Attributes.width
                            , imageSize |> String.fromFloat |> Svg.Attributes.height
                            ]

                Err err ->
                    err |> Json.Decode.errorToString |> Debug.log "error" |> Html.text
            , Html.button [ Html.Events.onClick RequestedNextImage ] [ Html.text "Next" ]
            , model.grammar |> Tracery.toString (\_ -> "null") |> Html.text
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        RequestedNextImage ->
            ( model, Random.generate NewSeed Random.independentSeed )

        Tick ->
            let
                ( grammar, seed ) =
                    Random.step
                        (model.grammar
                            |> Tracery.Grammar.generateNext Tracery.Grammar.defaultStrategy
                         -- |> Random.andThen (Tracery.runTo [ "square" ])
                        )
                        model.seed
            in
            ( { model | seed = seed, grammar = grammar }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 (\_ -> Tick)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
