module Clock exposing (main)

--import Html.Attributes exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Refined exposing (Refined, refine)
import Svg exposing (circle, line, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, viewBox, x1, x2, y1, y2)
import Time exposing (Time)
import Time.TimeZones exposing (europe_vienna)
import Time.ZonedDateTime exposing (ZonedDateTime, fromTimestamp, hour, millisecond, minute, second)
import Window exposing (resizes)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Refined Time
    , log : List String
    , size : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    let
        log : Result String (List String)
        log =
            Ok []

        time : Result String (Refined Time)
        time =
            refine 0 (\_ -> True)

        size : Result String Window.Size
        size =
            Ok { width = 0, height = 0 }

        model : Model
        model =
            case
                Result.map3 Model time log size
            of
                Ok a ->
                    a

                _ ->
                    Debug.crash "invalid initial state: "
    in
    model ! []



-- UPDATE


type Msg
    = Tick Time
    | Size Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                new_model =
                    case
                        model.time
                            |> Refined.set newTime
                    of
                        Ok a ->
                            { model | time = a }

                        Err str ->
                            { model | log = str :: model.log }
            in
            new_model ! []

        Size s ->
            { model | size = s } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every Time.second Tick
        , resizes (identity Size)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        time : ZonedDateTime
        time =
            Refined.get model.time
                |> fromTimestamp (europe_vienna ())

        hours =
            hour time

        minutes =
            minute time

        seconds =
            second time

        milli =
            millisecond time

        log : List String
        log =
            model.log

        angle =
            Refined.get model.time
                |> Time.inMinutes
                |> turns

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)
    in
    Grid.container []
        -- Responsive fixed width container
        [ CDN.stylesheet -- Inlined Bootstrap CSS for use with reactor
        , Grid.row []
            [ Grid.col []
                [ text (toString log) ]
            , Grid.col []
                [ div
                    [ style
                        [ ( "size", "1000px" )
                        , ( "background", "red" )
                        ]
                    ]
                    [ text "test" ]
                ]
            , Grid.col []
                [ text (toString hours ++ ":" ++ toString minutes)
                ]
            , Grid.col []
                [ svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
                    [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
                    , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
                    ]
                ]
            , Grid.col [] [ text (toString [ hours, minutes, seconds, milli ]) ]
            ]
        ]
