module HermeticMind.Logbook exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import HermeticMind.View.MarkdownRender as MarkdownRender
import Html exposing (Html)
import Http exposing (Error)
import Markdown.Block exposing (HeadingLevel)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer exposing (Renderer)


type alias Model =
    String


type Msg
    = GotResponse (Result Error String)


init : () -> ( Model, Cmd Msg )
init () =
    ( "loading...", getMarkdown GotResponse )


getMarkdown : (Result Error String -> Msg) -> Cmd Msg
getMarkdown gotResponse =
    Http.get
        { url =
            "https://raw.githubusercontent.com/Orasund/"
                ++ "elm-playground/master/src/HermeticMind/Markdown/logbook.md"
        , expect = Http.expectString gotResponse
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse result ->
            case result of
                Ok string ->
                    ( string, Cmd.none )

                Err err ->
                    let
                        _ =
                            err
                                |> Debug.log "Error"
                    in
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model |> Parser.parse of
        Ok list ->
            case list |> Renderer.render MarkdownRender.renderer of
                Ok elements ->
                    elements
                        |> Element.column
                            [ Element.width <| Element.px 800
                            , Element.centerX
                            , Element.padding 50
                            , Element.rgb255 255 255 255
                                |> Background.color
                            ]
                        |> Element.layout
                            [ Font.size 10
                            , Element.rgb255 64 64 64
                                |> Background.color
                            ]

                Err string ->
                    string |> Html.text

        Err list ->
            list
                |> List.map (Parser.deadEndToString >> Html.text)
                |> Html.div []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
