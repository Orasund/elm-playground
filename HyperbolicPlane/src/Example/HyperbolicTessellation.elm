module Example.HyperbolicTessellation exposing (..)

import Browser exposing (Document)
import Canvas
import Canvas.Settings
import Color
import Html exposing (Html)
import Hyperbolic exposing (BeltramiCoord, PoincareVector)
import Internal
import Svg
import Svg.Attributes
import Time


sizeOfGrid =
    0.842


type alias Model =
    { points : List ( PoincareVector, PoincareVector )
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
    in
    ( { points =
            [ ( Hyperbolic.fromPolarCoords { radius = sizeOfGrid, angle = 0 } |> Hyperbolic.toPoincareVector
              , Hyperbolic.fromPolarCoords { radius = sizeOfGrid, angle = 3 * pi / 2 } |> Hyperbolic.toPoincareVector
              )
            ]
      , pointsPerLine = 400
      , iter = 1
      , maxIter = maxIter
      }
    , Cmd.none
    )


newPoints : ( PoincareVector, PoincareVector ) -> List ( PoincareVector, PoincareVector )
newPoints ( p, fromP ) =
    let
        edges =
            5
    in
    List.range 1 (edges - 1)
        |> List.map toFloat
        |> List.map (\i -> i * 2 * pi / edges)
        |> List.map
            (\angle ->
                Hyperbolic.add
                    (p
                        |> Hyperbolic.negate
                        |> Hyperbolic.add fromP
                        |> Hyperbolic.rotateClockwise -angle
                    )
                    p
             --
            )
        --|> List.filter (\newP -> Hyperbolic.length p - Hyperbolic.length newP < 0.005)
        |> List.map
            (\newP ->
                ( newP
                , p
                )
            )


view : Model -> Document Msg
view model =
    { title = "Test"
    , body =
        (case model.points of
            ( head, offset ) :: _ ->
                ( head, offset )
                    |> (\t ->
                            [ 0, pi / 2, pi, 3 * pi / 2 ]
                                |> List.map
                                    (\amount ->
                                        t
                                            |> Tuple.mapBoth (Hyperbolic.rotateClockwise amount)
                                                (Hyperbolic.rotateClockwise amount)
                                    )
                       )
                    |> List.map (Tuple.mapBoth Hyperbolic.fromPoincareVector Hyperbolic.fromPoincareVector)
                    |> List.concatMap (Hyperbolic.pointsAlongLineSegment model.pointsPerLine)

            [] ->
                []
        )
            {--|> (++)
                ([ Hyperbolic.lineFromIdealPoints (Hyperbolic.pointAtInfinity 0) (Hyperbolic.pointAtInfinity pi)
                 , Hyperbolic.lineFromIdealPoints (Hyperbolic.pointAtInfinity (pi / 2)) (Hyperbolic.pointAtInfinity (3 * pi / 2))
                 ]
                    |> List.concatMap (Hyperbolic.pointsAlongLine model.pointsPerLine)
                )--}
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
                case model.points of
                    head :: tail ->
                        ( { model
                            | iter = model.iter + 1
                            , points = tail ++ newPoints head
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
