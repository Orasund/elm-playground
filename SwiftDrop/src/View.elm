module View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Layout
import View.Color


tile :
    { startDraggingFrom : ( Int, Int ) -> msg
    , dragTo : ( Int, Int ) -> msg
    , stopDragging : msg
    , dragging :
        Maybe
            { from : ( Int, Int )
            , to : ( Int, Int )
            }
    }
    -> ( ( Int, Int ), Maybe Int )
    -> Html msg
tile args ( pos, maybeValue ) =
    let
        ( x, y ) =
            pos

        width =
            100
    in
    (case maybeValue of
        Nothing ->
            ""

        Just value ->
            String.fromInt value
    )
        |> Html.text
        |> Layout.button
            ([ Html.Attributes.style "position" "absolute"
             , Html.Attributes.style "top" (String.fromInt (width * y) ++ "px")
             , Html.Attributes.style "left" (String.fromInt (width * x) ++ "px")
             , Html.Attributes.style "aspect-ratio" "1"
             , Html.Attributes.style "width" (String.fromInt width ++ "px")
             , Html.Attributes.style "border" "1px solid black"
             , Html.Attributes.style "background-color" (View.Color.fromInt (Maybe.withDefault 0 maybeValue))
             ]
                ++ Layout.centered
                ++ (case args.dragging of
                        Just { to } ->
                            if to == pos then
                                [ Html.Attributes.style "font-weight" "bold" ]

                            else if to /= pos then
                                [ args.dragTo pos
                                    |> Html.Events.onMouseEnter
                                ]

                            else
                                []

                        Nothing ->
                            []
                   )
            )
            { onPress =
                case args.dragging of
                    Just { to } ->
                        if to == pos then
                            args.stopDragging |> Just

                        else
                            args.dragTo pos |> Just

                    Nothing ->
                        args.startDraggingFrom pos |> Just
            , label =
                case maybeValue of
                    Nothing ->
                        "Empty"

                    Just value ->
                        String.fromInt value
            }
