module View.Info exposing (..)

import Data.Block exposing (Block)
import Data.Info exposing (Info)
import Html exposing (Html)
import Layout


toHtml : Info -> Html msg
toHtml info =
    [ Html.text info.title
        |> Layout.el []
    , (if info.content == [] then
        "Contains no items"

       else
        "Contains " ++ (info.content |> String.join ", ")
      )
        |> Html.text
        |> Layout.el []
    ]
        ++ (info.additionalInfo |> List.map (\string -> string |> Html.text |> Layout.el []))
        |> Layout.column []