module Main exposing (..)

import Browser exposing (Document)
import Dict
import Html
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Value exposing (JsonValue(..))
import Random exposing (Generator, Seed)
import Result.Extra
import Svg
import Svg.Attributes
import Tracery


type alias Model =
    { seed : Seed
    , circles : List ( ( Float, Float ), Circle )
    }


type Msg
    = NewSeed Seed
    | RequestedNextImage


type alias Circle =
    { size : Float, color : String }


generator : ( Float, Float ) -> Float -> Generator (Result Json.Decode.Error (List ( ( Float, Float ), Circle )))
generator pos size =
    Tracery.fromJson
        """
        { "origin":"#square3#"
        , "content":
            [ "\\"#color#\\""
            , "\\"#color#\\""
            , "#square#"
            ]
        , "color": ["\\\\#19381F","\\\\#EEE82C"]
        , "square":
            [ "#square3#"
            , "{\\"0\\":#content#,\\"2\\":#content#}"
            , "{\\"0\\":#content#,\\"3\\":#content#}"
            ]
        , "square3":
            [ "{\\"1\\":#content#,\\"2\\":#content#,\\"3\\":#square#}"
            , "{\\"0\\":#content#,\\"2\\":#square#,\\"3\\":#content#}"
            , "{\\"1\\":#square#,\\"0\\":#content#,\\"3\\":#content#}"
            , "{\\"1\\":#content#,\\"2\\":#content#,\\"0\\":#square#}"
            ]
        }
        """
        |> Result.Extra.extract (\err -> err |> Json.Decode.errorToString |> Random.constant)
        |> Random.map (Json.Decode.decodeString (circleDecoder pos size))


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
    ( { seed = Random.initialSeed 42, circles = [] }
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

            ( result, _ ) =
                Random.step (generator ( 50, 50 ) size) model.seed
        in
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
                            , "#91CB3E" |> Svg.Attributes.fill
                            ]
                            []
                        )
                    |> Svg.svg
                        [ imageSize |> String.fromFloat |> Svg.Attributes.width
                        , imageSize |> String.fromFloat |> Svg.Attributes.height
                        ]

            Err err ->
                err |> Json.Decode.errorToString |> Html.text
        , Html.button [ Html.Events.onClick RequestedNextImage ] [ Html.text "Next" ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        RequestedNextImage ->
            ( model, Random.generate NewSeed Random.independentSeed )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
