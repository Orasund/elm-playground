module GenerativeCard.Main exposing (main)

import Angle
import Axis2d exposing (Axis2d)
import Browser
import Dict exposing (Dict)
import Direction2d
import GenerativeCard.View as View
import Html exposing (Html)
import Html.Events as Events
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d, distanceFrom)
import Quantity exposing (Quantity)
import Random exposing (Generator, Seed, list)
import Svg
import Svg.Attributes as Attributes
import Vector2d exposing (Vector2d)


width : Float
width =
    800


height : Float
height =
    800


smallesLineSize : Float
smallesLineSize =
    10


type alias Model =
    { iteration : Int
    , path : List (Point2d Pixels ())
    , drawings : List ( Int, List ( Point2d Pixels (), Point2d Pixels () ) )
    , working : Bool
    , seed : Seed
    }


type Msg
    = Step


init : () -> ( Model, Cmd Msg )
init () =
    let
        path =
            [ Point2d.pixels (width / 4) (height / 4)
            , Point2d.pixels (width * 3 / 4) (height / 4)
            , Point2d.pixels (width * 3 / 4) (height * 3 / 4)
            , Point2d.pixels (width / 4) (height * 3 / 4)
            , Point2d.pixels (width / 4) (height / 4)
            ]

        iteration =
            1
    in
    ( { iteration = iteration
      , path = path
      , drawings =
            case path of
                [] ->
                    []

                head :: tail ->
                    [ ( iteration
                      , tail
                            |> List.foldl
                                (\to ( from, list ) ->
                                    ( to
                                    , ( to, from ) :: list
                                    )
                                )
                                ( head, [] )
                            |> Tuple.second
                      )
                    ]
      , working = False
      , seed = Random.initialSeed 42
      }
    , Cmd.none
    )


findSmallestDistanceTo : Point2d Pixels () -> List ( Point2d Pixels (), Point2d Pixels () ) -> Maybe Float
findSmallestDistanceTo target list =
    case list of
        [] ->
            Nothing

        _ ->
            list
                |> List.foldl
                    (\( from, to ) dist ->
                        Axis2d.throughPoints from to
                            |> Maybe.map
                                (\axis ->
                                    let
                                        distanceAlong =
                                            target
                                                |> Point2d.signedDistanceAlong axis
                                                |> Pixels.toFloat
                                    in
                                    (if distanceAlong < 0 then
                                        target
                                            |> Point2d.distanceFrom from

                                     else if
                                        distanceAlong
                                            > (from |> Point2d.distanceFrom to |> Pixels.toFloat)
                                     then
                                        target |> Point2d.distanceFrom to

                                     else
                                        target
                                            |> Point2d.signedDistanceFrom axis
                                    )
                                        |> Pixels.toFloat
                                        |> (\d ->
                                                if abs d < 0.0005 then
                                                    dist

                                                else if d < 0 then
                                                    dist
                                                        |> Tuple.mapFirst
                                                            (Maybe.map (min (abs d))
                                                                >> Maybe.withDefault (abs d)
                                                                >> Just
                                                            )

                                                else
                                                    dist
                                                        |> Tuple.mapSecond
                                                            (Maybe.map (min (abs d))
                                                                >> Maybe.withDefault (abs d)
                                                                >> Just
                                                            )
                                           )
                                )
                            |> Maybe.withDefault dist
                    )
                    ( Nothing, Nothing )
                |> (\out ->
                        case out of
                            ( Nothing, Nothing ) ->
                                Nothing

                            ( Nothing, Just n ) ->
                                Just n

                            ( Just n, Nothing ) ->
                                Just n

                            ( Just a, Just b ) ->
                                if a > b then
                                    Just -a

                                else
                                    Just b
                   )


step :
    Point2d Pixels ()
    ->
        Generator
            { from : Point2d Pixels ()
            , path : List (Point2d Pixels ())
            , iter : Int
            , searchList : List ( Point2d Pixels (), Point2d Pixels () )
            , drawings : List ( Point2d Pixels (), Point2d Pixels () )
            }
    ->
        Generator
            { from : Point2d Pixels ()
            , path : List (Point2d Pixels ())
            , iter : Int
            , searchList : List ( Point2d Pixels (), Point2d Pixels () )
            , drawings : List ( Point2d Pixels (), Point2d Pixels () )
            }
step to =
    Random.andThen
        (\{ from, iter, path, searchList, drawings } ->
            let
                mid =
                    Point2d.midpoint from to

                minLength =
                    Vector2d.from from mid |> Vector2d.length |> Pixels.toFloat

                defaultCase =
                    Random.constant
                        { from = mid
                        , path = path
                        , iter = iter
                        , searchList = searchList
                        , drawings = drawings
                        }
            in
            if minLength < smallesLineSize then
                defaultCase

            else
                case searchList |> findSmallestDistanceTo mid of
                    Nothing ->
                        defaultCase

                    Just radius ->
                        Random.float 0 pi
                            |> Random.map
                                (\r ->
                                    let
                                        scaleTo : Float -> Vector2d Pixels () -> Vector2d Pixels ()
                                        scaleTo len v =
                                            v
                                                |> Vector2d.scaleBy
                                                    (len
                                                        / (v
                                                            |> Vector2d.length
                                                            |> Pixels.toFloat
                                                          )
                                                    )

                                        p1 : Point2d Pixels ()
                                        p1 =
                                            if radius < 0 then
                                                mid
                                                    |> Point2d.translateBy
                                                        (Vector2d.from mid to
                                                            |> scaleTo (abs radius)
                                                            |> Vector2d.rotateBy (Angle.radians r)
                                                        )

                                            else
                                                mid
                                                    |> Point2d.translateBy
                                                        (Vector2d.from mid to
                                                            |> scaleTo (abs radius)
                                                            |> Vector2d.rotateBy (Angle.radians (r + pi))
                                                        )
                                    in
                                    { from = to
                                    , path = p1 :: from :: path
                                    , iter = iter
                                    , searchList = ( p1, to ) :: ( from, p1 ) :: searchList
                                    , drawings =
                                        ( p1, to ) :: ( from, p1 ) :: drawings
                                    }
                                )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            let
                iteration =
                    model.iteration + 1

                ( { path, drawings }, seed ) =
                    Random.step
                        (case model.path of
                            [] ->
                                Random.constant
                                    { path = model.path
                                    , drawings = []
                                    }

                            head :: tail ->
                                tail
                                    |> List.foldl step
                                        (Random.constant
                                            { from = head
                                            , path = []
                                            , iter = iteration
                                            , searchList =
                                                model.drawings
                                                    |> List.map Tuple.second
                                                    |> List.concat
                                            , drawings = []
                                            }
                                        )
                                    |> Random.map
                                        (\out ->
                                            { path = out.from :: out.path
                                            , drawings = out.drawings
                                            }
                                        )
                        )
                        model.seed
            in
            ( { model
                | path = path
                , iteration = iteration
                , drawings = ( iteration, drawings ) :: model.drawings
                , seed = seed
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    [ model.drawings
        |> List.map
            (\( iter, lines ) ->
                lines
                    |> List.map
                        ((\( from, to ) -> LineSegment2d.from from to)
                            >> View.line { iteration = iter, maxIter = model.iteration }
                        )
            )
        |> List.concat
        |> Svg.svg
            [ Attributes.width <| String.fromFloat width ++ "px"
            , Attributes.height <| String.fromFloat height ++ "px"
            , Attributes.version <| "1.1"
            ]
    , [ Html.text "Next Iteration"
      ]
        |> Html.button [ Events.onClick Step ]
    ]
        |> Html.div
            []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
