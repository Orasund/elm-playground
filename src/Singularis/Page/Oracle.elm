module Singularis.Page.Oracle exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation exposing (Key)
import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form.Decoder as Decoder exposing (Decoder)
import Html.Events as Events
import Json.Decode as Decode
import Singularis.Data as Data exposing (questionMinLength)
import Singularis.Data.Answer as Answer exposing (Answer)
import Singularis.View as View exposing (maxScreenWidth)
import Singularis.View.Answer as Answer
import Singularis.View.Element as Element
import Time exposing (Posix)


type alias Question =
    String


type alias QuestionForm =
    String


type QuestionFormError
    = QuestionTooShort


questionDecoder : Decoder String QuestionFormError String
questionDecoder =
    Decoder.custom
        (\input ->
            if questionMinLength > (input |> String.length) then
                Err [ QuestionTooShort ]

            else
                Ok <| String.replace "?" "" <| input
        )


type alias Model =
    { question : QuestionForm
    , errors : List QuestionFormError
    , answer : Maybe Answer
    }


type Msg
    = QuestionEntered String
    | QuestionAsked


init : Posix -> String -> Model
init time question =
    { question =
        if question == "" then
            ""

        else
            question ++ "?"
    , errors = []
    , answer =
        if question == "" then
            Nothing

        else
            let
                offset : Int
                offset =
                    Time.toDay Time.utc time
            in
            Just <| Answer.fromQuestion offset <| question
    }


update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg ({ question } as model) =
    case msg of
        QuestionEntered result ->
            ( { model | question = result }, Cmd.none )

        QuestionAsked ->
            case Decoder.run questionDecoder question of
                Ok result ->
                    ( model
                    , Navigation.pushUrl key ("?page=oracle&q=" ++ result)
                    )

                Err listOfErrors ->
                    ( { model | errors = listOfErrors }, Cmd.none )


viewError : QuestionFormError -> Element msg
viewError error =
    Element.el
        [ Element.width <| Element.fill
        , Border.rounded <| 10
        , Background.color <| Element.black
        , Font.color <| Element.white
        , Element.padding <| 10
        ]
    <|
        Element.paragraph [] <|
            List.singleton <|
                Element.text <|
                    case error of
                        QuestionTooShort ->
                            "The question needs to be at least "
                                ++ String.fromInt questionMinLength
                                ++ " characters long. Longer questions are generally better."


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


view : Float -> Model -> Element Msg
view scale { question, errors, answer } =
    Element.column
        [ Element.centerX
        , Element.width <| Element.fill
        , Element.spacing 20
        ]
    <|
        [ Element.section scale <| "Oracle of the fourth dimension"
        , Input.text
            [ Element.width <| Element.fill
            , Element.centerX
            , onEnter QuestionAsked
            ]
          <|
            { onChange = QuestionEntered
            , text = question
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    Element.text "Ask your question:"
            }
        ]
            ++ (errors
                    |> List.map viewError
               )
            ++ (case answer of
                    Nothing ->
                        []

                    Just a ->
                        [ Element.text "The oracle has answered:"
                        , Element.column
                            [ Element.centerX
                            , Element.spacing <| round <| (*) scale <| 10
                            , Element.width <|
                                Element.px <|
                                    (round <| (*) scale <| 300)
                            ]
                          <|
                            let
                                { name, desc } =
                                    Answer.desc <| a
                            in
                            [ Element.subsection scale <| name
                            , a
                                |> Answer.view
                                |> Element.html
                                |> Element.el [ Element.centerX ]
                            , Element.paragraph [ Font.center ] <|
                                List.singleton <|
                                    Element.text <|
                                        desc
                            ]
                        ]
               )
