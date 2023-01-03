module Main exposing (..)

import Browser exposing (Document)
import Canvas
import Canvas.Settings
import Color
import Html exposing (Html)
import Hyperbolic exposing (BeltramiCoords)
import Internal
import Svg
import Svg.Attributes
import Task
import Time


type alias Model =
    { lines :
        List
            { line : ( BeltramiCoords, BeltramiCoords )
            , center : BeltramiCoords
            , pointsPerLine : Int
            }
    , iter : Int
    , maxIter : Int
    }


type Msg
    = TimePassed


angleBetween : Float -> Float -> Float
angleBetween angle1 angle2 =
    let
        distance1 =
            maxP - minP

        distance2 =
            2 * pi - (maxP - minP)

        minP =
            min angle1 angle2

        maxP =
            max angle1 angle2
    in
    if distance1 >= pi then
        maxP + distance2 / 2

    else
        minP + distance1 / 2


trianglesBetween : ( Float, Float ) -> List ( Float, Float )
trianglesBetween ( angle1, angle2 ) =
    angleBetween angle1 angle2
        |> (\a -> [ ( angle1, a ), ( a, angle2 ) ])


init : () -> ( Model, Cmd Msg )
init () =
    ( { lines =
            let
                i1 =
                    0 |> Hyperbolic.pointAtInfinity |> Hyperbolic.fromIdealPoint

                i2 =
                    pi |> Hyperbolic.pointAtInfinity |> Hyperbolic.fromIdealPoint

                i3 =
                    2 * 2 * pi / 3 |> Hyperbolic.pointAtInfinity |> Hyperbolic.fromIdealPoint

                c =
                    Hyperbolic.origin
            in
            [ ( i1, c )
            , ( i2, c )
            ]
                |> List.map
                    (\line ->
                        { line = line
                        , center = Hyperbolic.origin
                        , pointsPerLine = 400
                        }
                    )
      , iter = 1
      , maxIter = 1000
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Test"
    , body =
        model.lines
            |> List.head
            |> Maybe.map
                (\{ line, pointsPerLine } ->
                    Hyperbolic.pointsAlongLineSegment pointsPerLine line
                )
            |> Maybe.withDefault []
            |> (\l ->
                    [ l
                        |> List.map Hyperbolic.toBeltramiCoordinates
                        |> viewAsCanvas
                    , l
                        |> List.map Hyperbolic.toPoincareCoordinates
                        |> viewAsCanvas
                    ]
               )
    }


viewAsSvg : List ( Float, Float ) -> Html msg
viewAsSvg list =
    list
        |> List.map
            (\( x, y ) ->
                Svg.circle
                    [ Svg.Attributes.r (String.fromFloat <| 0.5)
                    , Svg.Attributes.cx (String.fromFloat <| x * 20 + 50)
                    , Svg.Attributes.cy (String.fromFloat <| y * 20 + 50)
                    ]
                    []
            )
        |> Svg.svg
            [ Svg.Attributes.width "600"
            , Svg.Attributes.height "600"
            , Svg.Attributes.viewBox "0 0 100 100"
            ]


viewAsCanvas : List ( Float, Float ) -> Html msg
viewAsCanvas list =
    let
        width =
            600

        height =
            600

        zoom =
            300
    in
    list
        |> List.map
            (\( x, y ) ->
                Canvas.circle ( x * zoom + width / 2, y * zoom + height / 2 ) 1
            )
        |> Canvas.shapes [ Canvas.Settings.fill Color.black ]
        |> List.singleton
        |> Canvas.toHtml ( width, height ) []


euclideanDistance : BeltramiCoords -> BeltramiCoords -> Float
euclideanDistance p1 p2 =
    Internal.distance
        (Hyperbolic.toBeltramiCoordinates p1)
        (Hyperbolic.toBeltramiCoordinates p2)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePassed ->
            if model.iter < model.maxIter then
                ( { model
                    | iter = model.iter + 1
                    , lines =
                        case model.lines of
                            { line, center, pointsPerLine } :: tail ->
                                tail
                                    ++ (( line, center )
                                            |> (\( ( a1, a2 ), a3 ) ->
                                                    Hyperbolic.lineFromPoints a1 a2
                                                        |> (\l ->
                                                                l
                                                                    |> Hyperbolic.perpendicularLineThrough a3
                                                                    |> (\( i1, i2 ) ->
                                                                            Hyperbolic.intersectLines l ( i1, i2 )
                                                                                |> Maybe.map
                                                                                    (\z ->
                                                                                        let
                                                                                            p1 =
                                                                                                Hyperbolic.fromIdealPoint i1

                                                                                            p2 =
                                                                                                Hyperbolic.fromIdealPoint i2
                                                                                        in
                                                                                        [ ( ( p1, a1 ), a3 )
                                                                                        , ( ( p1, a2 ), a3 )
                                                                                        , ( ( p2, a1 ), a3 )
                                                                                        , ( ( p2, a2 ), a3 )
                                                                                        ]
                                                                                    )
                                                                       )
                                                                    |> Maybe.withDefault []
                                                           )
                                               )
                                            |> List.map
                                                (\( t, c ) ->
                                                    { line = t
                                                    , center = c
                                                    , pointsPerLine = pointsPerLine // 2
                                                    }
                                                )
                                       )

                            [] ->
                                []
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 300 (\_ -> TimePassed)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
