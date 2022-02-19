module Depp.Layout exposing (..)

{-| elm-layout

better than elm-ui

-}

import Html exposing (Html)
import Html.Attributes as Attr
import UndoList.Decode exposing (msg)


sidebarContainer :
    { sidebar : Html msg
    , main : Html msg
    }
    -> Html msg
sidebarContainer args =
    Html.div
        [ --Attr.class "container"
          Attr.style "display" "flex"
        ]
        [ Html.aside
            [ --Attr.class "container__sidebar"
              Attr.style "width" "30%"

            -- Make it scrollable
            , Attr.style "height" "100vh"
            , Attr.style "overflow" "auto"
            ]
            [ args.sidebar ]
        , Html.main_
            [ --Attr.class "container__main"
              -- Take the remaining width
              Attr.style "flex" "1"

            -- Make it scrollable
            , Attr.style "height" "100vh"
            , Attr.style "overflow" "auto"
            ]
            [ args.main ]
        ]


{-| for centering just use align-left and then center on your own.
-}
menu : List (List (Html msg)) -> Html msg
menu list =
    list
        |> List.map
            (\elem ->
                elem
                    |> Html.div
                        [ --Attr.class "menu__item"
                          -- Center the content horizontally
                          Attr.style "display" "flex"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "space-between"
                        ]
            )
        |> Html.div
            [ --Attr.class "menu"
              Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"

            -- Border
            , Attr.style "border" "1px solid rgba(0, 0, 0, 0.3)"
            , Attr.style "border-radius" "4px"
            ]


radioButtonGroup : List ( Bool, Html msg ) -> Html msg
radioButtonGroup buttons =
    buttons
        |> List.indexedMap
            (\i ( isSelected, button ) ->
                Html.label
                    ([ --Attr.class "container__label"
                       -- Center the content
                       Attr.style "align-items" "center"
                     , Attr.style "display" "inline-flex"
                     , Attr.style "padding" "8px"
                     ]
                        ++ (if i == 0 then
                                [ Attr.style "border-left" "1px solid rgba(0, 0, 0, 0.3)" ]

                            else
                                [ Attr.style "border-left" "1px solid transparent" ]
                           )
                        ++ (if isSelected then
                                [ -- For selected radio
                                  Attr.style "background-color" "#00449e"
                                , Attr.style "color" "#fff"
                                ]

                            else
                                [ -- For not selected radio
                                  Attr.style "background-color" "transparent"
                                , Attr.style "color" "#ccc"
                                ]
                           )
                    )
                    --for accesability
                    [ Html.input
                        [ Attr.type_ "radio"

                        --Attr.class "container__input"
                        , Attr.style "display" "none"
                        ]
                        []
                    , button
                    ]
            )
        |> Html.div
            [ -- Attr.class "container"
              Attr.style "display" "flex"

            -- Border
            , Attr.style "border" "ipx solid rgba(0, 0, 0, 0.3)"
            , Attr.style "border-radius" "4px"
            , Attr.style "height" "32px"
            ]


propertyItem : { name : Html msg, value : Html msg } -> Html msg
propertyItem args =
    [ Html.dt [] [ args.name ]
    , Html.dd [] [ args.value ]
    ]
        |> Html.dl
            [ --Attr.class "container"
              --Content is center horizontally
              Attr.style "align-items" "center"
            , Attr.style "display" "flex"

            --The property name will stick to the left, and the value
            --will stick to the right
            , Attr.style "justify-content" "space-between"
            , Attr.style "border-bottom" "1px solid rgba(0, 0, 0, 0.3)"

            -- Spacing
            , Attr.style "margin" "0px"
            , Attr.style "padding" "8px 0px"
            ]


chip : List (Html msg) -> Html msg
chip list =
    list
        |> List.map
            (\it -> Html.div [] [ it ])
        |> Html.div
            [ --Center the content
              Attr.style "align-items" "center"
            , Attr.style "display" "inline-flex"
            , Attr.style "justify-content" "center"

            --Background color
            , Attr.style "background-color" "rgba(0, 0, 0, 0.1)"

            --Rounded border
            , Attr.style "border-radius" "999999px"

            --Spacing
            , Attr.style "padding" "4px 8px"
            , Attr.style "gap" "4px"
            ]
