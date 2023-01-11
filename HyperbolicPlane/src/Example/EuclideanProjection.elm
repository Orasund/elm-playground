module Example.EuclideanProjection exposing (..)

import Browser exposing (Document)
import Canvas
import Canvas.Settings
import Color
import Html exposing (Html)
import Hyperbolic
import Svg
import Svg.Attributes
import Time


type alias Model =
    { points : List { radius : Float, angle : Float }
    , circleAngle : Float
    }


type Msg
    = TimePassed


pointsAround : ( Float, Float ) -> Float -> Int -> List ( Float, Float )
pointsAround p r n =
    List.range 0 (n - 1)
        |> List.map
            (\i ->
                pointAround
                    { point = p
                    , radius = r
                    , angle = 2 * pi * (toFloat i / (toFloat n - 1))
                    }
            )


pointAround : { point : ( Float, Float ), radius : Float, angle : Float } -> ( Float, Float )
pointAround args =
    let
        ( x, y ) =
            args.point
                |> fromPolar
    in
    fromPolar ( args.radius, args.angle )
        |> Tuple.mapBoth ((+) x) ((+) y)
        |> toPolar


init : () -> ( Model, Cmd Msg )
init () =
    ( { points =
            List.range 0 10
                |> List.concatMap
                    (\i ->
                        let
                            m =
                                i * 6
                        in
                        List.range 0 m
                            |> List.map
                                (\a ->
                                    { radius = toFloat i * 0.2
                                    , angle = 2 * pi * (toFloat a / toFloat m)
                                    }
                                )
                    )
      , circleAngle = 0
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Euclidean Projection"
    , body =
        model.points
            |> List.map
                (\center ->
                    pointAround
                        { point = ( center.radius, center.angle )
                        , radius = 0.1
                        , angle = model.circleAngle
                        }
                )
            |> List.map fromPolar
            |> (\l ->
                    [ l
                        |> List.map Hyperbolic.projectFromEuclideanSpace
                        |> List.map Hyperbolic.projectOntoBeltramiKleinDisc
                        |> viewAsCanvas
                    , l
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePassed ->
            ( { model
                | circleAngle = model.circleAngle + 0.01
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 20 (\_ -> TimePassed)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
