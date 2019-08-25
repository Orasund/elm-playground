module Singularis exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as BrowserEvents
import Browser.Navigation as Navigation exposing (Key)
import Color
import Element
import Element.Font as Font
import Geometry.Svg as Svg
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Markdown.Block as Block
import Random exposing (Seed)
import Singularis.Data.Answer as Answer exposing (list)
import Singularis.Page as Page exposing (Config, Route(..))
import Singularis.Page.Ai as Ai
import Singularis.Page.Home as Home
import Singularis.Page.Oracle as Oracle
import Singularis.View as View exposing (maxScreenWidth, minScreenWidth)
import Singularis.View.Answer as Answer
import Singularis.View.Element as Element
import Singularis.View.Polygon as Polygon
import Svg
import Svg.Attributes as Attributes
import Task
import Time exposing (Posix)
import Url exposing (Url)


type alias State =
    { config : Config
    , route : Route
    }


type alias ConfigBuilder =
    { url : Url
    , key : Key
    , time : Maybe Posix
    , scale : Maybe Float
    , seed : Maybe Seed
    , text : Maybe String
    }


type Model
    = Waiting ConfigBuilder
    | Done State


sizeToScale : Int -> Int -> Float
sizeToScale width _ =
    if toFloat width > maxScreenWidth then
        1

    else if toFloat width < minScreenWidth then
        minScreenWidth / maxScreenWidth

    else
        toFloat width / maxScreenWidth


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( Waiting
        { url = url
        , key = key
        , time = Nothing
        , scale = Nothing
        , seed = Nothing
        , text = Nothing
        }
    , Cmd.batch
        [ Task.perform (WaitingSpecific << GotTime) Time.now
        , Task.perform
            (\{ viewport } ->
                WaitingSpecific <|
                    GotSize (round viewport.width) (round viewport.height)
            )
            Dom.getViewport
        , Random.generate (WaitingSpecific << GotSeed) Random.independentSeed
        , Http.get
            { url =
                "https://raw.githubusercontent.com/Orasund/"
                    ++ "elm-playground/master/docs/"
                    ++ (url |> Page.getPageName)
                    ++ ".md"
            , expect =
                Http.expectString
                    (\result ->
                        case result of
                            Ok code ->
                                WaitingSpecific <| GotFile code

                            Err error ->
                                WaitingSpecific <| GotError error
                    )
            }
        ]
    )


type WaitingMsg
    = GotTime Posix
    | GotSize Int Int
    | GotSeed Seed
    | GotFile String
    | GotError Error


type Msg
    = OracleSpecific Oracle.Msg
    | WaitingSpecific WaitingMsg
    | AiSpecific Ai.Msg
    | UrlChanged Url
    | UrlRequested UrlRequest
    | SizeChanged Int Int


validateConfig : ConfigBuilder -> Model
validateConfig ({ key, url, time, scale, seed, text } as configBuilder) =
    case ( ( time, scale ), ( seed, text ) ) of
        ( ( Just posix, Just float ), ( Just s, Just t ) ) ->
            let
                config : Config
                config =
                    { key = key
                    , time = posix
                    , scale = float
                    , seed = s
                    , text = t
                    }
            in
            Done { config = config, route = url |> Page.extractRoute config }

        _ ->
            Waiting configBuilder


configUpdate : WaitingMsg -> ConfigBuilder -> Model
configUpdate msg config =
    validateConfig <|
        case msg of
            GotTime posix ->
                { config | time = Just posix }

            GotSize width height ->
                { config
                    | scale = Just <| sizeToScale width height
                }

            GotSeed seed ->
                { config
                    | seed = Just <| seed
                }

            GotFile text ->
                { config
                    | text = Just <| text
                }

            GotError error ->
                config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase =
            ( model, Cmd.none )
    in
    case model of
        Waiting config ->
            case msg of
                WaitingSpecific waitingMsg ->
                    ( config |> configUpdate waitingMsg, Cmd.none )

                _ ->
                    defaultCase

        Done ({ config } as state) ->
            case ( msg, state.route ) of
                ( OracleSpecific oracleMsg, Oracle oracleModel ) ->
                    oracleModel
                        |> Oracle.update config.key oracleMsg
                        |> Tuple.mapBoth
                            (Oracle >> (\route -> Done { state | route = route }))
                            (Cmd.map OracleSpecific)

                ( OracleSpecific _, _ ) ->
                    defaultCase

                ( AiSpecific aiMsg, Ai aiModel ) ->
                    ( Done
                        { state
                            | route =
                                aiModel
                                    |> Ai.update aiMsg
                                    |> Ai
                        }
                    , Cmd.none
                    )

                ( AiSpecific _, _ ) ->
                    defaultCase

                ( UrlChanged url, _ ) ->
                    ( Done { state | route = url |> Page.extractRoute config }
                    , Cmd.none
                    )

                ( UrlRequested urlRequest, _ ) ->
                    case urlRequest of
                        Internal url ->
                            ( model
                            , Navigation.pushUrl config.key (Url.toString url)
                            )

                        External url ->
                            ( model
                            , Navigation.load url
                            )

                ( SizeChanged width height, _ ) ->
                    ( Done
                        { state
                            | config = { config | scale = sizeToScale width height }
                        }
                    , Cmd.none
                    )

                ( WaitingSpecific _, _ ) ->
                    defaultCase


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.concat
            [ List.singleton <| BrowserEvents.onResize SizeChanged
            , case model of
                Done { route } ->
                    case route of
                        Ai aiModel ->
                            Ai.subscriptions aiModel
                                |> Sub.map AiSpecific
                                |> List.singleton

                        _ ->
                            []

                _ ->
                    []
            ]


view : Model -> Document Msg
view model =
    { title = "Occultus Singularis"
    , body =
        case model of
            Waiting _ ->
                []

            Done { route, config } ->
                List.singleton <|
                    Element.layout
                        []
                    <|
                        Element.column
                            [ Element.width <| Element.fill
                            , Element.spacing <| round <| (*) config.scale <| 10
                            ]
                        <|
                            [ Element.menu config.scale <|
                                [ { name = "Home", url = "?" }
                                , { name = "Oracle", url = "?page=oracle" }
                                , { name = "Singluarity", url = "?page=ai" }
                                ]
                            , Element.el
                                [ Element.width <|
                                    Element.px <|
                                        round <|
                                            (*) config.scale <|
                                                maxScreenWidth
                                , Element.centerX
                                , Element.padding <|
                                    round <|
                                        (*) config.scale <|
                                            10
                                ]
                              <|
                                Element.column [] <|
                                    (config.text
                                        |> Block.parse Nothing
                                        |> List.map (Element.fromMarkdown config.scale)
                                        |> (::)
                                            (case route of
                                                Home ->
                                                    Home.view config.scale

                                                Ai aiModel ->
                                                    Ai.view config.scale aiModel
                                                        |> Element.map AiSpecific

                                                Oracle oracleModel ->
                                                    Oracle.view config.scale oracleModel
                                                        |> Element.map OracleSpecific
                                            )
                                    )
                            ]
    }


onUrlRequest : UrlRequest -> Msg
onUrlRequest =
    UrlRequested


onUrlChange : Url -> Msg
onUrlChange =
    UrlChanged


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
