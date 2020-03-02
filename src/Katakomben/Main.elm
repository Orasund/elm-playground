module Katakomben.Main exposing (main)

import Action
import Browser
import Browser.Events exposing (onResize)
import Element
import Element.Font as Font
import Framework
import Html
import Html.Attributes as Attributes
import Katakomben.Page.Playing as Playing
import Katakomben.Page.Prepairing as Prepairing


type Model
    = Prepairing Prepairing.Model
    | Playing Playing.Model


type Msg
    = PlayingSpecific Playing.Msg
    | PrepairingSpecific Prepairing.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    Prepairing.init
        |> Tuple.mapBoth Prepairing (Cmd.map PrepairingSpecific)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PrepairingSpecific prepairingMsg, Prepairing prepairingModel ) ->
            Prepairing.update prepairingMsg prepairingModel
                |> Action.config
                |> Action.withUpdate Prepairing never
                |> Action.withTransition
                    (\seed ->
                        Playing.init seed
                    )
                    Playing
                    PlayingSpecific
                |> Action.apply

        ( PlayingSpecific playingMsg, Playing playingModel ) ->
            Playing.update playingMsg playingModel
                |> Action.config
                |> Action.withUpdate Playing PlayingSpecific
                |> Action.apply

        _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Playing playingModel ->
            playingModel
                |> Playing.subscriptions
                |> Sub.map PlayingSpecific

        _ ->
            Sub.none



----------------------
-- View
----------------------


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model of
                Playing playingModel ->
                    playingModel
                        |> Playing.view
                        |> List.map (Element.map PlayingSpecific)

                Prepairing prepairingModel ->
                    prepairingModel
                        |> Prepairing.view
    in
    { title = "Die Katakomben von Dunkelhall"
    , body =
        [ Html.node "meta"
            [ Attributes.attribute "name" "viewport"
            , Attributes.attribute "content" "width=device-width, initial-scale=1.0"
            , Attributes.id "game-body"
            , Attributes.tabindex 0
            ]
            []
        , Element.layoutWith
            { options = Framework.layoutOptions }
            (Framework.layoutAttributes
                ++ [ Font.family
                        [ Font.monospace
                        ]
                   ]
            )
          <|
            Element.column (Framework.container ++ [ Element.height <| Element.fill ]) <|
                content
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
