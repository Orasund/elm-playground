module View.Component exposing (..)

import CardGame.Card
import Html exposing (Attribute, Html)
import Html.Attributes


image : String
image =
    "https://upload.wikimedia.org/wikipedia/commons/f/f3/Elm_logo.svg"


defaultCard : List (Attribute msg) -> Html msg
defaultCard attrs =
    [ [ Html.div [] [ Html.text "Elm" ]
      , Html.div [] [ Html.text "🌳" ]
      ]
        |> CardGame.Card.header []
    , image |> CardGame.Card.fillingImage []
    , Html.text "removes runtime exceptions"
        |> CardGame.Card.description []
    ]
        |> CardGame.Card.card attrs


list : List ( String, Html msg ) -> Html msg
list l =
    l
        |> List.map
            (\( subtitle, content ) ->
                [ content
                , [ Html.text subtitle ]
                    |> Html.div [ Html.Attributes.style "text-align" "center" ]
                ]
                    |> Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "flex-direction" "column"
                        , Html.Attributes.style "gap" "8px"
                        ]
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "justify-content" "space-between"
            ]
