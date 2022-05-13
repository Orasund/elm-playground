module Penturtle.Main exposing (..)

import Array exposing (Array)
import Browser
import Color
import Html exposing (Html)
import Html.Attributes
import Quantity exposing (fractionalModBy)
import Queue exposing (Queue)
import Random exposing (Seed)
import Svg
import Svg.Attributes
import Time
import W.InputSlider exposing (color)


type Mode
    = Read
    | Write
    | Move


type alias Particle =
    { pos : ( Float, Float )
    , dir : ( Float, Float )
    , force : Float
    , path : Queue ( Float, Float )
    }


type alias Model =
    { particles : List Particle
    , strip : Array Bool
    , mode : Mode
    }


type Msg
    = Tick
    | NextAction
    | GotStrip (Array Bool)


maxRadius =
    50


distBetweenReaders : Int
distBetweenReaders =
    15


screenSize : Float
screenSize =
    600


circleSize : Float
circleSize =
    0.1


forceMultiplier : Float
forceMultiplier =
    0.05


friction : Float
friction =
    0.05


eps : Float
eps =
    0.01


init : () -> ( Model, Cmd Msg )
init () =
    ( { particles =
            List.range 0 4
                |> List.map (\int -> toFloat int * 2 * pi / 5 + pi / 2)
                |> List.map
                    (\angle ->
                        fromPolar ( maxRadius / 2, angle )
                            |> plus ( maxRadius / 2, maxRadius / 2 )
                    )
                |> List.map
                    (\pos ->
                        { pos = pos
                        , dir = ( 0, 0 )
                        , force = 0
                        , path =
                            pos
                                |> List.repeat 20
                                |> Queue.fromList
                        }
                    )
      , strip =
            Array.empty
      , mode = Read
      }
    , Random.uniform True [ False ]
        |> Random.list (5 * distBetweenReaders)
        |> Random.map Array.fromList
        |> Random.generate GotStrip
    )


plus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
plus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


view : Model -> Browser.Document Msg
view model =
    let
        color =
            Color.black
    in
    { title = "Penturtle"
    , body =
        [ [ model.particles
                |> List.map (\it -> ( it.pos, it.path |> Queue.toList ))
                |> List.map
                    (\( ( x, y ), list ) ->
                        Svg.path
                            [ list
                                |> List.reverse
                                |> List.map (\( i, j ) -> "L " ++ String.fromFloat i ++ " " ++ String.fromFloat j)
                                |> (::) ("M" ++ String.fromFloat x ++ " " ++ String.fromFloat y)
                                |> String.join " "
                                |> Svg.Attributes.d
                            , circleSize |> String.fromFloat |> Svg.Attributes.r
                            , color |> Color.toCssString |> Svg.Attributes.stroke
                            , "transparent" |> Svg.Attributes.fill
                            , circleSize |> String.fromFloat |> Svg.Attributes.strokeWidth
                            ]
                            []
                    )
                |> Svg.svg
                    [ screenSize |> String.fromFloat |> Svg.Attributes.width
                    , screenSize |> String.fromFloat |> Svg.Attributes.height
                    , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt maxRadius ++ " " ++ String.fromInt maxRadius)
                    ]
          ]
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                ]
        , (case model.mode of
            Read ->
                "Reading"

            Write ->
                "Writing"

            Move ->
                "Moving"
          )
            |> Html.text
        , model.strip
            |> Array.toIndexedList
            |> List.map
                (\( i, bool ) ->
                    let
                        text =
                            (if bool then
                                "1"

                             else
                                "0"
                            )
                                |> Html.text
                    in
                    if i |> modBy distBetweenReaders |> (==) 0 then
                        Html.span
                            [ Html.Attributes.style "background-color" (color |> Color.toCssString)
                            , Html.Attributes.style "color" (Color.white |> Color.toCssString)
                            ]
                            [ text ]

                    else
                        Html.span [] [ text ]
                )
            |> Html.div []
        ]
    }


getForce : List Particle -> Particle -> ( Float, Float )
getForce list particle =
    list
        |> List.map
            (\p ->
                let
                    ( pX, pY ) =
                        p.pos

                    ( x, y ) =
                        particle.pos

                    length =
                        sqrt ((pX - x) ^ 2 + (pY - y) ^ 2)

                    sign a =
                        a > 0

                    force =
                        if sign p.force == sign particle.force then
                            -(abs p.force)

                        else
                            abs p.force
                in
                if length < eps then
                    ( 0, 0 )

                else
                    ( ((pX - x) / length) * force * forceMultiplier
                    , ((pY - y) / length) * force * forceMultiplier
                    )
            )
        |> List.unzip
        |> Tuple.mapBoth List.sum List.sum


tick : Model -> ( Model, Cmd Msg )
tick model =
    ( { model
        | particles =
            model.particles
                |> List.map
                    (\particle ->
                        let
                            force =
                                particle
                                    |> getForce
                                        (centerForce particle
                                            :: model.particles
                                        )

                            dir =
                                particle.dir
                                    |> Tuple.mapBoth ((*) (1 - friction)) ((*) (1 - friction))
                                    |> plus force

                            centerForce a =
                                { pos = ( maxRadius / 2, maxRadius / 2 )
                                , dir = ( 0, 0 )
                                , force =
                                    a.pos
                                        |> toPolar
                                        |> (\( radius, angle ) ->
                                                if maxRadius / 2 < radius then
                                                    -a.force

                                                else
                                                    0
                                           )
                                , path = Queue.empty
                                }

                            ( posX, posY ) =
                                particle.pos
                        in
                        { particle
                            | pos = particle.pos |> plus dir
                            , dir = dir
                            , path =
                                particle.path
                                    |> Queue.dequeue
                                    |> Tuple.second
                                    |> Queue.enqueue particle.pos
                        }
                    )
      }
    , Cmd.none
    )


nextAction : Model -> ( Model, Cmd Msg )
nextAction m0 =
    ( { m0
        | mode =
            case m0.mode of
                Read ->
                    Write

                Write ->
                    Move

                Move ->
                    Read
      }
        |> (\model ->
                case model.mode of
                    Read ->
                        { model
                            | particles =
                                model.particles
                                    |> List.indexedMap
                                        (\i particle ->
                                            model.strip
                                                |> Array.get (i * distBetweenReaders)
                                                |> Maybe.map
                                                    (\bool ->
                                                        { particle
                                                            | force =
                                                                if bool then
                                                                    particle.force + 1

                                                                else
                                                                    particle.force - 1
                                                        }
                                                    )
                                                |> Maybe.withDefault particle
                                        )
                        }

                    Write ->
                        { model
                            | strip =
                                model.particles
                                    |> List.indexedMap Tuple.pair
                                    |> List.foldl
                                        (\( i, particle ) ->
                                            if particle.force > 0 then
                                                Array.set (i * distBetweenReaders) False

                                            else if particle.force < 0 then
                                                Array.set (i * distBetweenReaders) True

                                            else
                                                identity
                                        )
                                        model.strip
                        }

                    Move ->
                        { model
                            | strip =
                                case model.strip |> Array.toList of
                                    [] ->
                                        Array.empty

                                    head :: tail ->
                                        tail ++ [ head ] |> Array.fromList
                        }
           )
    , Cmd.none
    )


gotStrip : Array Bool -> Model -> ( Model, Cmd Msg )
gotStrip strip model =
    ( { model | strip = strip }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        Tick ->
            tick

        NextAction ->
            nextAction

        GotStrip strip ->
            gotStrip strip


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        second =
            1000
    in
    [ Time.every (second / 10) (\_ -> Tick)
    , Time.every second (\_ -> NextAction)
    ]
        |> Sub.batch


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
