module FactoryCity.View.Text exposing (view)

import Element exposing (Element)
import Emoji
import Html
import Html.Attributes as Attributes


view : Int -> String -> Element msg
view int string =
    let
        scale : Float
        scale =
            1.17

        diff : Float -> Float
        diff =
            (*) (scale - 1)
    in
    Element.html <|
        Html.span
            [ Attributes.class "elm-emoji"
            , Attributes.style "height" (String.fromInt int ++ "px")
            ]
        <|
            Emoji.textWith
                (\list ->
                    Html.img
                        [ Attributes.src <|
                            "https://openmoji.org/data/"
                                ++ "color"
                                ++ "/svg/"
                                ++ (List.intersperse "-" list |> String.join "" |> String.toUpper)
                                ++ ".svg"
                        , Attributes.height <| round <| (*) scale <| toFloat int
                        , Attributes.style "vertical-align" "middle"
                        ]
                        []
                )
            <|
                string
