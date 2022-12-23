module Main exposing (..)

import Browser exposing (Document)


type alias Model =
    { shape : List ( Float, Float ) }


init : () -> ( Model, Cmd () )
init () =
    ( {}, Cmd.none )


view : Model -> Document ()
view model =
    { title = "Fill with Text"
    , body = [ Html.text "Fill with Text" ]
    }


update : () -> Model -> ( Model, Cmd () )
update () model =
    ( model, Cmd.none )


subscriptions : Model -> Sub ()
subscriptions model =
    Sub.none


main : Program () Model ()
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
