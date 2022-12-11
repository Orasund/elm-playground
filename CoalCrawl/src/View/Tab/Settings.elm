module View.Tab.Settings exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Layout
import View.Button


percentRange : { name : String, value : Int, onInput : Maybe Int -> msg } -> Html msg
percentRange args =
    [ Html.text args.name
    , Html.input
        [ Attr.type_ "range"
        , Attr.min "0"
        , Attr.max "100"
        , Attr.value (String.fromInt args.value)
        , Events.onInput (\int -> int |> String.toInt |> args.onInput)
        ]
        []
    ]
        |> Layout.row [ Layout.spacing 8 ]


settings :
    { restart : msg
    , setVolume : Maybe Int -> msg
    , volume : Int
    , setZoom : Maybe Int -> msg
    , zoom : Int
    }
    -> Html msg
settings args =
    [ View.Button.toHtml (Just args.restart) "Restarts"
    , percentRange
        { name = "Volume"
        , value = args.volume
        , onInput = args.setVolume
        }
    , percentRange
        { name = "Zoom"
        , value = args.zoom
        , onInput = args.setZoom
        }
    ]
        |> Layout.column [ Layout.spacing 8 ]
