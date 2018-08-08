module Clock exposing (main)

--import Html.Attributes exposing (..)

import Css
import Css.Foreign as Foreign
import Date exposing (Date)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Task
import Time exposing (Time)
import Window


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
    { time : Date
    , width : Int
    , height : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        time : Date
        time =
            Date.fromTime 0

        width : Int
        width =
            0

        height : Int
        height =
            0
    in
    { time = time, width = width, height = height }
        ! [ Task.perform Resize Window.size
          , Task.perform Tick Time.now
          ]



-- UPDATE


type Msg
    = Tick Time
    | Resize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            { model | time = Date.fromTime time } ! []

        Resize { height, width } ->
            { model | height = height, width = width } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every Time.minute Tick
        , Window.resizes Resize
        ]



-- VIEW


color : Int -> Css.Color
color hour =
    case hour of
        12 ->
            Css.rgb 23 240 240

        11 ->
            Css.rgb 255 178 25

        10 ->
            Css.rgb 95 49 243

        9 ->
            Css.rgb 255 255 25

        8 ->
            Css.rgb 246 24 174

        7 ->
            Css.rgb 24 248 24

        6 ->
            Css.rgb 255 129 25

        5 ->
            Css.rgb 46 106 243

        4 ->
            Css.rgb 255 216 25

        3 ->
            Css.rgb 163 36 243

        2 ->
            Css.rgb 177 253 25

        _ ->
            Css.rgb 255 25 25


circle : Int -> { height : Int, width : Int } -> List Css.Style -> List (Html Msg) -> Html Msg
circle size { height, width } styleList htmlList =
    Html.div
        [ css
            ([ Css.position Css.fixed
             , Css.top <| Css.px <| toFloat <| (height - size) // 2
             , Css.left <| Css.px <| toFloat <| (width - size) // 2
             , Css.width <| Css.px <| toFloat <| size
             , Css.height <| Css.px <| toFloat <| size
             , Css.borderRadius <| Css.px <| toFloat <| size // 2
             ]
                |> List.append styleList
            )
        ]
        htmlList


view : Model -> Html Msg
view ({ time, height, width } as model) =
    let
        size =
            max height width

        hours =
            Date.hour time

        minutes =
            Date.minute time

        seconds =
            Date.second time

        milli =
            Date.millisecond time

        angle =
            time
                |> Date.toTime
                |> Time.inMinutes
                |> turns

        handX =
            toString (50 + 40 * cos angle)

        handY =
            toString (50 + 40 * sin angle)
    in
    Html.div
        [ css
            [ Css.width <| Css.pct <| 100
            , Css.height <| Css.px <| toFloat <| height
            , Css.backgroundColor <| Css.rgb 0 0 0
            ]
        ]
        [ Foreign.global
            [ Foreign.selector
                "@keyframes puls"
                [ Css.property "0% { opacity:1 } 50% { opacity:0.25 } 100% { opacity:1 };" "" ]
            ]
        , circle
            size
            { height = height, width = width }
            [ Css.backgroundColor <| Css.rgb 0 0 0
            , Css.border3 (Css.px <| 1) Css.solid (Css.rgb 63 63 63)
            ]
            []
        , circle
            (if hours == 5 || hours == 6 then
                size // 2 + (size * minutes) // (60 * 2)
             else
                size // 2
            )
            { height = height, width = width }
            [ Css.backgroundColor <| Css.rgb 255 255 255
            , if hours >= 22 || hours <= 4 then
                Css.visibility <| Css.hidden
              else
                Css.visibility <| Css.visible
            ]
            []
        , circle (size // 2)
            { height = height, width = width }
            [ Css.backgroundColor <| color (hours % 12)
            , Css.property "animation" "puls 2.0s infinite"
            , if hours >= 22 || hours <= 4 then
                Css.visibility <| Css.hidden
              else
                Css.visibility <| Css.visible
            ]
            []
        , Html.div
            [ css
                [ Css.textAlign Css.center
                , Css.fontSize <| Css.px <| toFloat <| size // 5
                , Css.top <| Css.px <| toFloat <| height // 2 - size // 8
                , Css.position <| Css.fixed
                , Css.margin <| Css.auto
                , Css.width <| Css.pct <| 100
                , if hours >= 22 || hours <= 4 then
                    Css.color <| color (hours % 12)
                  else
                    Css.color <| Css.rgb 0 0 0
                ]
            ]
            [ Html.text
                (toString hours
                    ++ ":"
                    ++ (if minutes < 10 then
                            "0"
                        else
                            ""
                       )
                    ++ toString minutes
                )
            ]
        ]
