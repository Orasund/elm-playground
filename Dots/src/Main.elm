module Main exposing (..)

import Browser exposing (Document)
import Dict
import Html
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Value exposing (JsonValue(..))
import Random exposing (Seed)
import String
import Svg exposing (Svg)
import Svg.Attributes
import Time
import Tracery
import Tracery.Command exposing (Command(..))
import Tracery.Grammar exposing (Grammar)
import Tracery.Syntax exposing (Expression(..))


backgroundColor : String
backgroundColor =
    "#1C1018"


color1 : String
color1 =
    "#95E06C"


color2 : String
color2 =
    "#68B684"


color3 : String
color3 =
    "#094D92"


type alias Model =
    { seed : Seed
    , grammar : Grammar
    , circles : List ( ( Float, Float ), Shape )
    , error : Maybe String
    }


type Msg
    = NewSeed Seed
    | RequestedNextImage
    | Tick


type ShapeType
    = Circle
    | Square


type alias Shape =
    { size : Float, color : String, shapeType : ShapeType }


generateGrammar : Result Json.Decode.Error Grammar
generateGrammar =
    """
        { "origin":"{\\"0\\":#square#,\\"1\\":#square#,\\"2\\":#square#,\\"3\\":#square#}"
        , "content":
            [ "#element#"
            , "#element#"
            , "null"
            , "#square#"
            ]
        , "element":[ "[\\"#shape#\\",\\"#color#\\"]"]
        , "shape": ["square","circle","circle"]
        , "color": ["""
        ++ ([ List.repeat 8 color1
            , List.repeat 8 color2
            , [ color3 ]
            ]
                |> List.concat
                |> List.map (\string -> "\"\\\\" ++ string ++ "\"")
                |> String.join ","
           )
        ++ """]
        , "arrangement1":
            ["{\\"0\\":null,\\"1\\":#content#,\\"2\\":#content#,\\"3\\":#square#}"
            ,"{\\"0\\":#content#,\\"1\\":null,\\"2\\":#square#,\\"3\\":#content#}"
            ,"{\\"0\\":#content#,\\"1\\":#square#,\\"2\\":null,\\"3\\":#content#}"
            ,"{\\"0\\":#square#,\\"1\\":#content#,\\"2\\":#content#,\\"3\\":null}"
            ]
        , "arrangement2":
            ["{\\"0\\":null,\\"1\\":null,\\"2\\":#content#,\\"3\\":#content#}"
            ,"{\\"0\\":#content#,\\"1\\":null,\\"2\\":null,\\"3\\":#content#}"
            ,"{\\"0\\":#content#,\\"1\\":#content#,\\"2\\":null,\\"3\\":null}"
            ,"{\\"0\\":null,\\"1\\":#content#,\\"2\\":#content#,\\"3\\":null}"
            ,"{\\"0\\":null,\\"1\\":#content#,\\"2\\":null,\\"3\\":#content#}"
            ,"{\\"0\\":#content#,\\"1\\":null,\\"2\\":#content#,\\"3\\":null}"
            ]
        , "arr1":"#arrangement1#"
        , "arr2":"#arrangement2#"
        , "square":
            [ "{\\"0\\":#content#,\\"1\\":#content#,\\"2\\":#content#,\\"3\\":#content#}"
            , "#arr1#"
            ,"#arr2#"
            ]
        }
        """
        |> Tracery.fromJson


toCircles : ( Float, Float ) -> Float -> Grammar -> Result Json.Decode.Error (List ( ( Float, Float ), Shape ))
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


shapeTypeFromString : String -> Maybe ShapeType
shapeTypeFromString string =
    case string of
        "circle" ->
            Just Circle

        "square" ->
            Just Square

        _ ->
            Nothing


circleList : { pos : ( Float, Float ), size : Float } -> JsonValue -> List ( ( Float, Float ), Shape )
circleList args jsonValue =
    let
        ( x1, y1 ) =
            args.pos
    in
    case jsonValue of
        ArrayValue list ->
            case list of
                [ StringValue shapeType, StringValue color ] ->
                    [ ( args.pos
                      , { size = args.size
                        , color = color
                        , shapeType = shapeType |> shapeTypeFromString |> Maybe.withDefault Circle
                        }
                      )
                    ]

                _ ->
                    []

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


circleDecoder : ( Float, Float ) -> Float -> Decoder (List ( ( Float, Float ), Shape ))
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


viewShape : ( ( Float, Float ), Shape ) -> Svg msg
viewShape ( ( x, y ), shape ) =
    case shape.shapeType of
        Circle ->
            Svg.circle
                [ x + shape.size / 2 |> String.fromFloat |> Svg.Attributes.cx
                , y + shape.size / 2 |> String.fromFloat |> Svg.Attributes.cy
                , shape.size / 2 |> String.fromFloat |> Svg.Attributes.r
                , shape.color |> Svg.Attributes.fill
                , "drop-shadow(0px 4px 2px rgb(0 0 0 / 0.4))"
                    |> Svg.Attributes.filter
                ]
                []

        Square ->
            Svg.polygon
                [ [ [ x + shape.size / 2, y ]
                  , [ x + shape.size, y + shape.size / 2 ]
                  , [ x + shape.size / 2, y + shape.size ]
                  , [ x, y + shape.size / 2 ]
                  ]
                    |> List.map
                        (\list ->
                            list
                                |> List.map String.fromFloat
                                |> String.join ","
                        )
                    |> String.join " "
                    |> Svg.Attributes.points
                , shape.color |> Svg.Attributes.fill
                , "drop-shadow(0px 4px 2px rgb(0 0 0 / 0.4))"
                    |> Svg.Attributes.filter
                ]
                []


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
                        |> List.map viewShape
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
                            |> Random.andThen (Tracery.runTo [ "square" ])
                        )
                        model.seed
            in
            ( { model | seed = seed, grammar = grammar }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 (\_ -> Tick)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
