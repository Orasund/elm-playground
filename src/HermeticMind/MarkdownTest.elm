module HermeticMind.MarkdownTest exposing (main)

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as Attributes


main : Html msg
main =
    [ Html.div [ Attributes.attribute "style" "page-break-after:always;page-break-before:always;" ]
        [ Html.text "test1 " ]
        |> Element.html
    , Html.div [ Attributes.attribute "style" "page-break-after:always;page-break-before:always;" ]
        [ Html.text "test2 " ]
        |> Element.html
    , Html.div [ Attributes.attribute "style" "page-break-after:always;page-break-before:always;" ]
        [ Html.text "test3 " ]
        |> Element.html
    ]
        |> Element.column []
        |> Element.layoutWith { options = [ Element.noStaticStyleSheet ] } []
