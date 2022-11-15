module View.Info exposing (..)

import Data.Info exposing (Info)
import Html exposing (Html)
import Layout


justContent : List String -> Html msg
justContent content =
    [ (if content == [] then
        "Contains no items"

       else
        "Contains " ++ (content |> String.join ", ")
      )
        |> Html.text
        |> Layout.el []
    ]
        |> Layout.column []


toHtml : Info -> Html msg
toHtml info =
    [ "Selected: "
        ++ info.title
        |> Html.text
    , Html.text info.description
        |> Layout.paragraph []
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
