module View.Button exposing (..)

import ElmBook exposing (Msg)
import ElmBook.Actions
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event


view : { label : String, onPress : state -> state } -> Html (Msg state)
view args =
    Html.button
        [ Event.onClick
            (args.onPress
                |> ElmBook.Actions.updateState
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "border-size" "0"
        , Attr.style "padding" "0px 8px"
        , Attr.class "elm-book-md__default"
        ]
        [ Html.text args.label
        ]
