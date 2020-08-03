module Ecocards.Main exposing (main)

import Browser
import Ecocards.Page.LocalGame as LocalGame
import Element
import Html exposing (Html)
import Random exposing (Seed)


type Msg
    = GetSeed Seed
    | LocalGameMsg LocalGame.Msg


type Model
    = Waiting
    | LocalGameModel LocalGame.Model


init : ( Model, Cmd Msg )
init =
    ( Waiting, Random.generate GetSeed Random.independentSeed )


update : Msg -> Model -> ( Model, Cmd Msg )
update a b =
    case ( a, b ) of
        ( GetSeed seed, Waiting ) ->
            LocalGame.init seed
                |> Tuple.mapBoth
                    LocalGameModel
                    (Cmd.map LocalGameMsg)

        ( LocalGameMsg msg, LocalGameModel model ) ->
            model
                |> LocalGame.update msg
                |> Tuple.mapBoth
                    LocalGameModel
                    (Cmd.map LocalGameMsg)

        _ ->
            ( b, Cmd.none )


view : Model -> Html Msg
view m =
    case m of
        Waiting ->
            Html.div [] []

        LocalGameModel model ->
            LocalGame.view model
                |> Element.layout []
                |> Html.map LocalGameMsg


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
