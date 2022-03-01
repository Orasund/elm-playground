module Ruz.Main exposing (..)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html
import Layout
import Ruz.Config as Config


type alias Model =
    { grid : Dict ( Int, Int ) () }


type Msg
    = Toggle ( Int, Int )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = Dict.empty }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Ruz Puzzle"
    , body =
        [ List.repeat Config.size ()
            |> List.indexedMap
                (\i () ->
                    List.repeat Config.size ()
                        |> List.indexedMap
                            (\j () ->
                                Html.text ""
                            )
                        |> Layout.row []
                )
            |> Layout.column []
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle pos ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
