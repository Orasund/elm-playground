module HermeticMind.Logbook exposing (main)

import Browser
import Css
import HermeticMind.View.MarkdownRender as MarkdownRender
import Html as UnstyledHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
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


view : Model -> UnstyledHtml.Html Msg
view model =
    case model |> Parser.parse of
        Ok list ->
            case list |> Renderer.render MarkdownRender.renderer of
                Ok elements ->
                    elements
                        |> Html.div
                            [ Attributes.css
                                [ Css.width <| Css.px 800
                                , Css.margin2 (Css.px 0) Css.auto
                                , Css.backgroundColor (Css.rgb 255 255 255)
                                ]
                            ]
                        |> List.singleton
                        |> Html.div
                            [ Attributes.css
                                [ Css.fontSize (Css.px 10)
                                , Css.backgroundColor (Css.rgb 64 64 64)
                                ]
                            ]
                        |> Html.toUnstyled

                Err string ->
                    string |> UnstyledHtml.text

        Err list ->
            list
                |> List.map (Parser.deadEndToString >> UnstyledHtml.text)
                |> UnstyledHtml.div []


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
