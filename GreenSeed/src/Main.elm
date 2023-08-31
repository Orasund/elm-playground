module Main exposing (..)

import Browser
import Cell
import Crop exposing (Crop)
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Overlay exposing (Overlay(..))
import View
import View.Game
import View.MenuBar
import View.Overlay


type alias Model =
    { game : Game
    , overlay : Maybe Overlay
    }


type Msg
    = SelectPos ( Int, Int )
    | SelectCrop (Maybe Crop)
    | SetOverlay (Maybe Overlay)
    | EndDay


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , overlay = Nothing
      }
    , Cmd.none
    )


setGameOf : Model -> Game -> Model
setGameOf model game =
    { model | game = game }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPos pos ->
            ( case model.overlay of
                Just (ShopOverlay (Just crop)) ->
                    model.game
                        |> Game.buyAndPlace pos crop
                        |> Maybe.map
                            (setGameOf
                                { model
                                    | overlay = Nothing
                                }
                            )
                        |> Maybe.withDefault model

                _ ->
                    model
            , Cmd.none
            )

        SelectCrop crop ->
            case model.overlay of
                Just (ShopOverlay _) ->
                    ( { model | overlay = ShopOverlay crop |> Just }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetOverlay overlay ->
            ( { model | overlay = overlay }, Cmd.none )

        EndDay ->
            ( model.game
                |> Game.dayPassed
                |> setGameOf model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewOverlay : Model -> Html Msg
viewOverlay model =
    case model.overlay of
        Just (ShopOverlay maybeCrop) ->
            model.game.forSale
                |> View.Overlay.asShop maybeCrop SelectCrop

        Nothing ->
            Layout.none


view : Model -> Html Msg
view model =
    [ View.Game.toHtml [] { selectPos = SelectPos } model.game
    , viewOverlay model
    , View.MenuBar.toHtml { openShop = SetOverlay (Just (ShopOverlay Nothing)), sleep = EndDay }
    , View.stylesheet
    ]
        |> Html.div [ Html.Attributes.style "position" "relative" ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
