module FactoryCity.View.Text exposing (black, colored)

import Element exposing (Element)
import Emoji
import Html
import Html.Attributes as Attributes


black : Int -> String -> Element msg
black size text =
    view
        { sort = "black"
        , text = text
        , size = size
        }


colored : Int -> String -> Element msg
colored size text =
    view
        { sort = "color"
        , text = text
        , size = size
        }


view : { sort : String, size : Int, text : String } -> Element msg
view { sort, size, text } =
    let
        scale : Float
        scale =
            1.17
    in
    Element.html <|
        Html.span
            [ Attributes.class "elm-emoji"
            , Attributes.style "height" (String.fromInt size ++ "px")
            ]
        <|
            Emoji.textWith
                (\list ->
                    Html.img
                        [ Attributes.src <|
                            "https://openmoji.org/data/"
                                ++ sort
                                ++ "/svg/"
                                ++ (List.intersperse "-" list |> String.join "" |> String.toUpper)
                                ++ ".svg"
                        , Attributes.height <| round <| (*) scale <| toFloat size
                        , Attributes.style "vertical-align" "middle"
                        ]
                        []
                )
            <|
                text
