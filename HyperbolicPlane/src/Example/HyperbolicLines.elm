module Example.HyperbolicLines exposing (..)

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
    { lines : List { line : ( Float, Float ), pointsPerLine : Int }
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
            [ ( 0, 2 * pi / 3 ), ( 2 * pi / 3, 4 * pi / 3 ), ( 4 * pi / 3, 0 ) ]
                |> List.map (\line -> { line = line, pointsPerLine = 400 })
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Hyperbolic Lines"
    , body =
        model.lines
            |> List.head
            |> Maybe.map
                (\{ line, pointsPerLine } ->
                    let
                        ( angle1, angle2 ) =
                            line
                    in
                    ( Hyperbolic.lineFromIdealPoints (Hyperbolic.pointAtInfinity angle1)
                        (Hyperbolic.pointAtInfinity angle2)
                    , pointsPerLine
                    )
                )
            |> Maybe.map
                (\( line, pointsPerLine ) ->
                    line
                        |> Tuple.mapBoth Hyperbolic.unsafeFromIdealPoint Hyperbolic.unsafeFromIdealPoint
                        |> Hyperbolic.pointsAlongLineSegment pointsPerLine
                )
            |> Maybe.withDefault []
            |> (\l ->
                    [ l
                        |> List.map Hyperbolic.projectOntoBeltramiKleinDisc
                        |> viewAsCanvas
                    , l
                        |> List.map Hyperbolic.projectOntoPoincareDisc
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
                | lines =
                    case model.lines of
                        { line, pointsPerLine } :: tail ->
                            tail
                                ++ (trianglesBetween line
                                        |> List.map
                                            (\l ->
                                                { line = l
                                                , pointsPerLine = pointsPerLine // 2
                                                }
                                            )
                                   )

                        [] ->
                            []
              }
            , Cmd.none
            )


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
