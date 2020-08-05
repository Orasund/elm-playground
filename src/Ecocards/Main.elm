module Ecocards.Main exposing (main)

import Browser
import Ecocards.Page.LocalGame as LocalGame
import Element
import Html exposing (Html)
import Html.Attributes as Attributes
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
    Html.div []
        (Html.node "meta"
            [ Attributes.attribute "name" "viewport"
            , Attributes.attribute "content" "width=device-width, initial-scale=1.0, user-scalable=no"
            ]
            []
            :: (case m of
                    Waiting ->
                        []

                    LocalGameModel model ->
                        [ LocalGame.view model
                            |> Html.map LocalGameMsg
                        ]
               )
        )


subscriptions : Model -> Sub Msg
subscriptions m =
    case m of
        Waiting ->
            Sub.none

        LocalGameModel model ->
            LocalGame.subscriptions model |> Sub.map LocalGameMsg


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
