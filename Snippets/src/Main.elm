module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Model =
    { value : Maybe Int }


type Msg
    = StartSpinning
    | StopSpinning Int


init : () -> ( Model, Cmd Msg )
init () =
    ( { value = Nothing }, Cmd.none )


sideName : Int -> String
sideName int =
    case int of
        1 ->
            "showOne"

        2 ->
            "showTwo"

        3 ->
            "showThree"

        4 ->
            "showFour"

        5 ->
            "showFive"

        6 ->
            "showSix"

        _ ->
            ""


view : Model -> Html Msg
view model =
    let
        diceClasses =
            case model.value of
                Just a ->
                    "dice " ++ sideName a

                Nothing ->
                    "dice spinning"
    in
    [ Html.div
        [ Html.Attributes.class diceClasses
        ]
        [ Html.div [ Html.Attributes.class "side one" ] [ Html.text "One" ]
        , Html.div [ Html.Attributes.class "side two" ] [ Html.text "Two" ]
        , Html.div [ Html.Attributes.class "side three" ] [ Html.text "Three" ]
        , Html.div [ Html.Attributes.class "side four" ] [ Html.text "Four" ]
        , Html.div [ Html.Attributes.class "side five" ] [ Html.text "Five" ]
        , Html.div [ Html.Attributes.class "side six" ] [ Html.text "Six" ]
        ]
        |> List.singleton
        |> Html.div
            [ Html.Attributes.style "height" "400px"
            ]
    , List.range 1 6
        |> List.map
            (\value ->
                Html.button [ Html.Events.onClick (StopSpinning value) ]
                    [ Html.text (String.fromInt value)
                    ]
            )
        |> Html.div []
    , Html.div []
        [ Html.button [ Html.Events.onClick StartSpinning ]
            [ Html.text "spin" ]
        ]
    , Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.type_ "text/css"
        , Html.Attributes.href "style.css"
        ]
        []
    ]
        |> Html.div []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartSpinning ->
            ( { value = Nothing }, Cmd.none )

        StopSpinning value ->
            ( { value = Just value }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
