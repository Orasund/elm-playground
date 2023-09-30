module View.Artefact exposing (..)

import Artefact exposing (Artefact)
import Html exposing (Html)
import Html.Attributes
import Layout


toHtml : { onActivate : Artefact -> msg } -> List Artefact -> Html msg
toHtml args list =
    list
        |> List.map
            (\artefact ->
                [ Artefact.name artefact
                    |> Layout.text [ Html.Attributes.style "font-size" "1.2rem" ]
                , Artefact.description artefact
                    |> Layout.text []
                , Layout.textButton []
                    { label = "Activate"
                    , onPress = args.onActivate artefact |> Just
                    }
                ]
                    |> Layout.column [ Html.Attributes.style "border" "1px solid black" ]
            )
        |> Layout.column []


noButtons : Artefact -> Html msg
noButtons artefact =
    [ Artefact.name artefact
        |> Layout.text [ Html.Attributes.style "font-size" "1.2rem" ]
    , Artefact.description artefact
        |> Layout.text []
    ]
        |> Layout.column [ Html.Attributes.style "border" "1px solid black" ]
