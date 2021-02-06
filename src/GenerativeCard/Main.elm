module GenerativeCard.Main exposing (main)

import Angle
import Axis2d exposing (Axis2d)
import Browser
import Dict exposing (Dict)
import Direction2d
import GenerativeCard.Computation as Computation exposing (Computation(..))
import GenerativeCard.View as View
import Html exposing (Html)
import Html.Events as Events
import Html.Events.Extra.Mouse as Mouse exposing (Event)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d, distanceFrom)
import Process
import Quantity exposing (Quantity)
import Random exposing (Generator, Seed, list)
import Svg
import Svg.Attributes as Attributes
import Task
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


width : Float
width =
    800


height : Float
height =
    800


optDurationInMillis : Float
optDurationInMillis =
    50


type alias DrawingModel =
    List (Point2d Pixels ())


type alias ReadyModel =
    { iteration : Int
    , path : List (List (Point2d Pixels ()))
    , drawings : List ( Int, List ( Point2d Pixels (), Point2d Pixels () ) )
    , seed : Seed
    , computation : Maybe Computation
    }


type Model
    = Drawing DrawingModel
    | Ready ReadyModel


type DrawingMsg
    = MouseClicked Event
    | FinishedDrawing


type ReadyMsg
    = NextIteration
    | Step Posix


type Msg
    = WhileDrawing DrawingMsg
    | WhileReady ReadyMsg


init : () -> ( Model, Cmd Msg )
init () =
    ( Drawing
        [ Point2d.pixels (width / 4) (height / 4)
        , Point2d.pixels (width * 3 / 4) (height / 4)
        , Point2d.pixels (width * 3 / 4) (height * 3 / 4)
        , Point2d.pixels (width / 4) (height * 3 / 4)
        ]
    , Cmd.none
    )


createDrawings : Int -> List (Point2d Pixels ()) -> List ( Int, List ( Point2d Pixels (), Point2d Pixels () ) )
createDrawings iteration path =
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


transition : DrawingModel -> ReadyModel
transition drawingModel =
    let
        iteration =
            1
    in
    { iteration = iteration
    , path = drawingModel |> List.singleton
    , drawings = drawingModel |> createDrawings iteration
    , seed = Random.initialSeed 42
    , computation = Nothing
    }


updateDrawing : DrawingMsg -> DrawingModel -> ( Model, Cmd Msg )
updateDrawing msg model =
    case msg of
        FinishedDrawing ->
            ( (case model of
                [] ->
                    []

                head :: tail ->
                    (head :: tail)
                        ++ [ head ]
              )
                |> transition
                |> Ready
            , Cmd.none
            )

        MouseClicked { pagePos } ->
            ( Drawing
                (Point2d.pixels (pagePos |> Tuple.first) (pagePos |> Tuple.second)
                    :: model
                )
            , Cmd.none
            )


selectN : Int -> List (List a) -> ( List (List a), List (List a) )
selectN n list =
    case list of
        [] ->
            ( [], [] )

        head :: tail ->
            if (head |> List.length) > n then
                ( head |> List.take n |> List.singleton, (head |> List.drop n) :: tail )

            else if (head |> List.length) == n then
                ( head |> List.singleton, tail )

            else if head |> List.isEmpty then
                selectN n tail

            else
                selectN (n - (head |> List.length)) tail
                    |> (\( a, b ) ->
                            ( head :: a, b )
                       )


updateReady : ReadyMsg -> ReadyModel -> ( ReadyModel, Cmd ReadyMsg )
updateReady msg model =
    case msg of
        NextIteration ->
            case model.path of
                (head :: _) :: _ ->
                    let
                        iteration =
                            model.iteration + 1
                    in
                    ( { model
                        | iteration = iteration
                        , computation =
                            Just
                                (Running
                                    { remaining = model.path
                                    , result =
                                        { from = head
                                        , currentPath = []
                                        , previousPaths = []
                                        , iter = iteration
                                        , searchList =
                                            model.drawings
                                                |> List.map Tuple.second
                                                |> List.concat
                                        , drawings = []
                                        }
                                    , lastUpdate = Nothing
                                    , chunkSize = 1
                                    }
                                )
                      }
                    , Time.now |> Task.perform Step
                    )

                _ ->
                    ( model, Cmd.none )

        Step time ->
            case model.computation of
                Just (Running { remaining, result, lastUpdate, chunkSize }) ->
                    case lastUpdate of
                        Nothing ->
                            if remaining |> List.isEmpty then
                                ( { model
                                    | computation = Just <| Done result
                                  }
                                , Time.now |> Task.perform Step
                                )

                            else
                                let
                                    ( selected, newRemaining ) =
                                        remaining |> selectN chunkSize

                                    ( newResult, seed ) =
                                        Random.step
                                            (selected
                                                |> List.foldl
                                                    (\l c ->
                                                        l |> List.foldl Computation.step c
                                                    )
                                                    (Random.constant result)
                                            )
                                            model.seed
                                in
                                ( { model
                                    | computation =
                                        Just <|
                                            Running
                                                { remaining = newRemaining
                                                , result = newResult
                                                , lastUpdate = Just time
                                                , chunkSize = chunkSize
                                                }
                                    , seed = seed
                                  }
                                , Time.now |> Task.perform Step
                                )

                        Just lastTime ->
                            case remaining of
                                [] ->
                                    ( { model
                                        | computation = Just <| Done result
                                      }
                                    , Time.now |> Task.perform Step
                                    )

                                currentPath :: remainingPaths ->
                                    let
                                        duration =
                                            (time |> Time.posixToMillis)
                                                - (lastTime |> Time.posixToMillis)
                                                |> toFloat

                                        newChunkSize =
                                            5

                                        ( selected, newRemaining ) =
                                            remaining |> selectN newChunkSize

                                        ( newResult, seed ) =
                                            Random.step
                                                (selected
                                                    |> List.foldl
                                                        (\l c ->
                                                            l |> List.foldl Computation.step c
                                                        )
                                                        (Random.constant result)
                                                )
                                                model.seed
                                    in
                                    ( { model
                                        | computation =
                                            Just <|
                                                Running
                                                    { remaining = newRemaining
                                                    , result = newResult
                                                    , lastUpdate = Just time
                                                    , chunkSize = newChunkSize
                                                    }
                                        , seed = seed
                                      }
                                    , Process.sleep 10
                                        |> Task.andThen (\() -> Time.now)
                                        |> Task.perform Step
                                    )

                Just (Done result) ->
                    let
                        { path, drawings } =
                            { path = (result.from :: result.currentPath) :: result.previousPaths
                            , drawings = result.drawings
                            }
                    in
                    ( { model
                        | path = path
                        , drawings = ( model.iteration, drawings ) :: model.drawings
                        , computation = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WhileDrawing ms, Drawing ml ) ->
            updateDrawing ms ml

        ( WhileReady ms, Ready ml ) ->
            updateReady ms ml
                |> Tuple.mapBoth Ready (Cmd.map WhileReady)

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        drawings =
            case model of
                Drawing path ->
                    path |> createDrawings 1

                Ready m ->
                    m.drawings

        iteration =
            case model of
                Drawing _ ->
                    1

                Ready m ->
                    m.iteration
    in
    [ drawings
        |> List.map
            (\( iter, lines ) ->
                lines
                    |> List.map
                        ((\( from, to ) -> LineSegment2d.from from to)
                            >> View.line { iteration = iter, maxIter = iteration }
                        )
            )
        |> List.concat
        |> Svg.svg
            [ Attributes.width <| String.fromFloat width ++ "px"
            , Attributes.height <| String.fromFloat height ++ "px"
            , Attributes.version <| "1.1"
            ]
    , case model of
        Drawing _ ->
            [ Html.text "Finish Drawing"
            ]
                |> Html.button [ Events.onClick (FinishedDrawing |> WhileDrawing) ]

        Ready { computation } ->
            case computation of
                Nothing ->
                    [ Html.text "Next Iteration"
                    ]
                        |> Html.button [ Events.onClick (NextIteration |> WhileReady) ]

                Just comp ->
                    Html.text <|
                        "Running ("
                            ++ (case comp of
                                    Running { remaining } ->
                                        remaining
                                            |> List.concat
                                            |> List.length
                                            |> String.fromInt

                                    Done _ ->
                                        "0"
                               )
                            ++ " remaining , "
                            ++ (case comp of
                                    Running { remaining } ->
                                        remaining
                                            |> List.length
                                            |> String.fromInt

                                    Done _ ->
                                        "0"
                               )
                            ++ "chunks remaining)"
    , Html.text <| String.fromInt iteration ++ ". Iteration"
    ]
        |> Html.div
            (case model of
                Drawing _ ->
                    [ Mouse.onClick (MouseClicked >> WhileDrawing) ]

                Ready _ ->
                    []
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
