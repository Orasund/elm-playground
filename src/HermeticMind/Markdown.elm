module HermeticMind.Markdown exposing (main)

import Browser
import Css
import Direction2d
import HermeticMind.Data.Alphabet as Alphabet
import HermeticMind.View.BinarySigil as BinarySigil
import HermeticMind.View.BraidSigil as BraidSigil
import HermeticMind.View.GreekMagicSymbol as GreekMagicSymbol
import HermeticMind.View.MagicSquareSigil as MagicSquareSigil
import HermeticMind.View.MarkdownRender as MarkdownRender
import Html as UnstyledHtml
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http exposing (Error)
import Markdown.Block exposing (HeadingLevel)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer exposing (Renderer)
import Point2d
import Quantity
import Svg
import Svg.Attributes as SvgAttributes


type alias Model =
    String


type Msg
    = GotResponse (Result Error String)


file =
    "constructionOfSigils"



--"logbook"


init : () -> ( Model, Cmd Msg )
init () =
    ( "loading...", getMarkdown GotResponse )


getMarkdown : (Result Error String -> Msg) -> Cmd Msg
getMarkdown gotResponse =
    Http.get
        { url =
            "https://raw.githubusercontent.com/Orasund/"
                ++ "elm-playground/master/src/HermeticMind/Markdown/"
                ++ file
                ++ ".md"
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


interactive : String -> List (Html msg) -> Html msg
interactive name content =
    (case name of
        "greekMagicSymbols" ->
            Alphabet.asList
                |> List.filterMap
                    (GreekMagicSymbol.fromChar 16
                        >> Maybe.map
                            (Html.fromUnstyled
                                >> List.singleton
                                >> Html.span
                                    [ Attributes.css
                                        [ Css.padding2 (Css.px 0) (Css.px 1)
                                        ]
                                    ]
                            )
                    )
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        , Css.margin4 (Css.px 14) (Css.px 0) (Css.px 0) (Css.px 0)
                        ]
                    ]
                |> List.singleton

        "BinarySigil" ->
            let
                width : Float
                width =
                    40

                height : Float
                height =
                    55

                --64
            in
            List.range 0 (2 ^ 2 - 1)
                |> List.map
                    (\value ->
                        BinarySigil.view
                            { point =
                                Point2d.unsafe
                                    { x = width / 2, y = height / 2 }
                            , value = value
                            , size = 2
                            , color = "black"
                            , radius = 4
                            , strokeWidth = 2
                            , direction = Direction2d.positiveX
                            }
                            |> Svg.svg
                                [ SvgAttributes.width <| (String.fromFloat <| width) ++ "px"
                                , SvgAttributes.height <| (String.fromFloat <| height) ++ "px"
                                , SvgAttributes.version <| "1.1"
                                , SvgAttributes.viewBox <|
                                    "0 0 "
                                        ++ String.fromFloat width
                                        ++ " "
                                        ++ String.fromFloat height
                                ]
                            |> Html.fromUnstyled
                    )
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        ]
                    ]
                |> List.singleton

        "MagicSquareSigil" ->
            "Hermetic Mind"
                |> MagicSquareSigil.view
                    { size = 128
                    , zoom = 1
                    , strokeWidth = 2
                    , alphabet = Alphabet.english
                    }
                |> Html.fromUnstyled
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        , Css.margin4 (Css.px 8) (Css.px 0) (Css.px 0) (Css.px 0)
                        ]
                    ]
                |> List.singleton

        "BraidSigil" ->
            "Hermetic Mind"
                |> BraidSigil.view
                    { width = 256
                    , height = 256
                    , radius = 100
                    , zoom = 1
                    , asAlphabet = Alphabet.english
                    , withCircle = True
                    , debugMode = False
                    , withRunes = False
                    , withText = False
                    , withBorder = False
                    }
                |> Html.fromUnstyled
                |> List.singleton
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.justifyContent Css.center
                        , Css.margin4 (Css.px 8) (Css.px 0) (Css.px 0) (Css.px 0)
                        ]
                    ]
                |> List.singleton

        _ ->
            Html.text "Content not found"
                |> List.singleton
    )
        |> (\list ->
                list
                    ++ (content
                            |> Html.div
                                [ Attributes.css
                                    [ Css.displayFlex
                                    , Css.justifyContent Css.center
                                    ]
                                ]
                            |> List.singleton
                       )
           )
        |> Html.div
            [ Attributes.css
                [ Css.border3 (Css.px 1) Css.dashed (Css.rgb 0 0 0)
                , Css.padding (Css.px 8)
                , Css.borderRadius (Css.px 8)
                , Css.margin2 (Css.px 0) (Css.px 32)
                ]
            ]


view : Model -> UnstyledHtml.Html Msg
view model =
    case model |> Parser.parse of
        Ok list ->
            case
                list
                    |> Renderer.render
                        (MarkdownRender.renderer interactive)
            of
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
                                [ Css.fontSize (Css.px 14)
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
