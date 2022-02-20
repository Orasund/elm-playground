module Depp.View exposing (actionGroup, actionSelect, actions, card, collection, listing, selectButton, singleButton, stylesheet)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events
import UndoList.Decode exposing (msg)


stylesheet : Html msg
stylesheet =
    Html.node "link"
        [ Attr.rel "stylesheet"

        --Pico
        , Attr.href "https://unpkg.com/@picocss/pico@latest/css/pico.min.css"

        --NES UI
        --, Attr.href "https://unpkg.com/nes.css@2.3.0/css/nes.min.css"
        -- RPGUI
        --, Attr.href "https://raw.githubusercontent.com/RonenNess/RPGUI/master/dist/rpgui.min.css"
        -- MVP
        --, Attr.href "https://unpkg.com/mvp.css"
        ]
        []


card :
    { title : String
    , actions : Maybe ( { label : String, onClick : Maybe msg }, List { label : String, onClick : Maybe msg } )
    }
    -> Html msg
    -> Html msg
card args content =
    [ Html.h4 [] [ Html.text args.title ]
    , content
        :: (case args.actions of
                Just ( primary, list ) ->
                    actions primary list
                        |> List.singleton

                Nothing ->
                    []
           )
        |> Html.p []
    ]
        |> Html.aside []
        |> List.singleton
        |> Html.section []


listing : String -> List (Html msg) -> Html msg
listing title list =
    list
        |> List.map
            (\content ->
                content
                    |> List.singleton
                    |> Html.p []
            )
        |> (::) (Html.summary [] [ Html.text title ])
        |> Html.details [ Attr.attribute "open" "true" ]


collection : String -> List (Html msg) -> Html msg
collection title list =
    Html.details [ Attr.attribute "open" "true" ]
        [ Html.summary [] [ Html.text title ]
        , list
            |> internalRow
            |> List.singleton
            |> Html.p []
        ]


actions : { label : String, onClick : Maybe msg } -> List { label : String, onClick : Maybe msg } -> Html msg
actions primary list =
    let
        primaryButton =
            internalButton True primary

        listButtons =
            list |> List.map (internalButton False)
    in
    if (list |> List.length) < 3 then
        [ primaryButton ]
            ++ listButtons
            |> internalRow

    else
        Html.details [ Attr.attribute "open" "true" ]
            [ Html.summary [] [ primaryButton ]
            , listButtons
                |> internalRow
                |> List.singleton
                |> Html.p []
            ]


actionGroup : String -> List { label : String, onClick : Maybe msg } -> Html msg
actionGroup title list =
    list
        |> List.map (internalButton False)
        |> collection title


actionSelect : String -> List ( Bool, { label : String, onClick : Maybe msg } ) -> Html msg
actionSelect title list =
    list
        |> List.map (\( isSelected, button ) -> internalButton isSelected button)
        |> collection title


singleButton : { label : String, onClick : Maybe msg } -> Html msg
singleButton =
    internalButton True


selectButton : ( Bool, { label : String, onClick : Maybe msg } ) -> Html msg
selectButton ( isSelected, button ) =
    internalButton isSelected button



-------------------------------------------------------------------------------
-- INTNERAL
-------------------------------------------------------------------------------


internalBaseFlex : List (Attribute msg)
internalBaseFlex =
    [ Attr.style "display" "flex"
    , Attr.style "flex-wrap" "wrap"
    , Attr.style "gap" "10px"
    ]


internalColumn : List (Html msg) -> Html msg
internalColumn =
    Html.div
        (internalBaseFlex
            ++ [ Attr.style "flex-direction" "column"
               ]
        )


internalRow : List (Html msg) -> Html msg
internalRow =
    Html.div
        (internalBaseFlex
            ++ [ Attr.style "flex-direction" "row"
               ]
        )


internalButton : Bool -> { label : String, onClick : Maybe msg } -> Html msg
internalButton isPrimary args =
    Html.a
        ([ Attr.attribute "role" "button"
         , Attr.href "#"
         ]
            ++ (if isPrimary then
                    []

                else
                    [ Attr.class "outline" ]
               )
            ++ (args.onClick
                    |> Maybe.map
                        (\msg ->
                            msg
                                |> Events.onClick
                                |> List.singleton
                        )
                    |> Maybe.withDefault []
               )
        )
        [ Html.text args.label ]
