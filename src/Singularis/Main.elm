module Singularis exposing (main)

import Browser exposing (Document,UrlRequest(..))
import Color
import Element
import Element.Font as Font
import Geometry.Svg as Svg
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Singularis.Data.Answer as Answer exposing (list)
import Singularis.Page as Page exposing (Route(..))
import Singularis.Page.Home as Home
import Singularis.Page.Oracle as Oracle
import Singularis.View as View exposing (maxScreenWidth)
import Singularis.View.Answer as Answer
import Singularis.View.Element as Element
import Singularis.View.Polygon as Polygon
import Svg
import Svg.Attributes as Attributes
import Browser.Navigation as Navigation exposing (Key)
import Url exposing (Url)

type alias Model =
    { key : Key
    , route : Route
    }


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, route = url  |> Page.extractRoute}, Cmd.none )


type Msg
    = OracleSpecific Oracle.Msg
    | UrlChanged Url
    | UrlRequested UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({key} as model) =
    let
        defaultCase : ( Model, Cmd Msg )
        defaultCase = (model,Cmd.none)
    in
    case (msg,model.route) of
        (OracleSpecific oracleMsg,Oracle oracleModel) ->
                oracleModel
                |> Oracle.update key oracleMsg
                |> Tuple.mapBoth (Oracle >> \route -> {model|route=route}) (Cmd.map OracleSpecific)
        
        (OracleSpecific _,_) ->
            defaultCase

        (UrlChanged url,_) ->
            ({model | route = url |> Page.extractRoute  },Cmd.none)
        (UrlRequested urlRequest,_) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Navigation.load url
                    )

subscriptions : model -> Sub msg
subscriptions _=
    Sub.none

view : Model -> Document Msg
view {route} =
    { title = "Singularis"
    , body =
        List.singleton <|
        Element.layout
            []
        <|
            Element.column [ Element.width <| Element.fill
            , Element.spacing 10  ] <|
                [ Element.menu <|
                    [ {name= "Home", url="?"}
                    , {name="Oracle",url="?page=oracle"}
                    ]
                , Element.el
                    [ Element.width <| Element.px <| maxScreenWidth
                    , Element.centerX
                    ]
                  <|
                    case route of
                        Home ->
                            Home.view
                        Oracle oracleModel ->
                            Oracle.view oracleModel
                            |> Element.map OracleSpecific
                ]
    }

onUrlRequest : UrlRequest -> Msg
onUrlRequest = UrlRequested

onUrlChange : Url -> Msg
onUrlChange = UrlChanged

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
