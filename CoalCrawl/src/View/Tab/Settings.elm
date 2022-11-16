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
    , slowedDown : Bool
    , toggleSlowdown : msg
    , setVolume : Maybe Int -> msg
    , volume : Int
    , setZoom : Maybe Int -> msg
    , zoom : Int
    }
    -> Html msg
settings args =
    [ [ (if args.slowedDown then
            "Stop Slow Motion"

         else
            "Start Slow Motion"
        )
            |> View.Button.toHtml (Just args.toggleSlowdown)
      , View.Button.toHtml (Just args.restart) "Restarts"
      ]
        |> Layout.row [ Layout.spacing 8 ]
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
