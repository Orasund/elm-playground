module Example.LineIntersection exposing (..)

import Browser exposing (Document)
import Canvas
import Canvas.Settings
import Color
import Html exposing (Html)
import Html.Attributes exposing (size)
import Hyperbolic exposing (BeltramiCoord, IdealPoint, Line)
import Internal
import Svg
import Svg.Attributes
import Time


type alias Model =
    { lines : List ( IdealPoint, IdealPoint )
    , pointsPerLine : Int
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
    let
        maxIter =
            1000

        i1 =
            pi

        i2 =
            pi + 2 * pi / 3

        i3 =
            pi + 4 * pi / 3
    in
    ( { lines =
            [ ( i1, i2 ), ( i3, i1 ) ]
                |> List.map (Tuple.mapBoth Hyperbolic.pointAtInfinity Hyperbolic.pointAtInfinity)
      , pointsPerLine = 400
      , iter = 1
      , maxIter = maxIter
      }
    , Cmd.none
    )


newPoint : Line -> Maybe ( BeltramiCoord, IdealPoint )
newPoint line =
    line
        |> Hyperbolic.perpendicularLineThrough (Hyperbolic.unsafeFromIdealPoint (Hyperbolic.pointAtInfinity 0))
        |> Maybe.andThen
            (\( i1, i2 ) ->
                Hyperbolic.intersectLines line ( i1, i2 )
                    |> Maybe.map
                        (\c ->
                            if euclideanDistance (Hyperbolic.unsafeFromIdealPoint i1) c < euclideanDistance (Hyperbolic.unsafeFromIdealPoint i2) c then
                                ( c, i1 )

                            else
                                ( c, i2 )
                        )
            )


view : Model -> Document Msg
view model =
    { title = "Line Intersection"
    , body =
        (case model.lines of
            line :: _ ->
                (line |> Hyperbolic.pointsAlongLine model.pointsPerLine)
                    ++ (line
                            |> newPoint
                            |> Maybe.map (Tuple.mapSecond Hyperbolic.unsafeFromIdealPoint)
                            |> Maybe.map (Hyperbolic.pointsAlongLineSegment model.pointsPerLine)
                            |> Maybe.withDefault []
                       )

            [] ->
                []
        )
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
        size =
            600

        zoom =
            300
    in
    [ list
        |> List.map
            (\( x, y ) ->
                Canvas.circle ( x * zoom + size / 2, y * zoom + size / 2 ) 1
            )
        |> Canvas.shapes [ Canvas.Settings.fill Color.black ]
    , Canvas.circle ( size / 2, size / 2 ) zoom
        |> List.singleton
        |> Canvas.shapes [ Canvas.Settings.stroke Color.black ]
    ]
        |> Canvas.toHtml ( size, size ) []


euclideanDistance : BeltramiCoord -> BeltramiCoord -> Float
euclideanDistance p1 p2 =
    Internal.distance
        (Hyperbolic.projectOntoBeltramiKleinDisc p1)
        (Hyperbolic.projectOntoBeltramiKleinDisc p2)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePassed ->
            if model.iter < model.maxIter then
                case model.lines of
                    ( i1, i2 ) :: tail ->
                        ( { model
                            | iter = model.iter + 1
                            , lines =
                                tail
                                    ++ (newPoint ( i1, i2 )
                                            |> Maybe.map Tuple.second
                                            |> Maybe.map (\i3 -> [ ( i1, i3 ), ( i3, i2 ) ])
                                            |> Maybe.withDefault []
                                       )
                          }
                        , Cmd.none
                        )

                    [] ->
                        ( model, Cmd.none )

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
