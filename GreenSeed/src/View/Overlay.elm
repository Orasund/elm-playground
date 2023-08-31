module View.Overlay exposing (..)

import Crop exposing (Crop)
import Html exposing (Html)
import Layout
import Overlay exposing (Overlay(..))
import View.Crop


asShop : Maybe Crop -> (Maybe Crop -> msg) -> List Crop -> Html msg
asShop maybeCrop onSelect crops =
    [ Html.text "Shop"
    , case maybeCrop of
        Just crop ->
            crop
                |> View.Crop.asCircle
                    (Layout.asButton
                        { label = "Select Crop"
                        , onPress = onSelect Nothing |> Just
                        }
                    )
                |> Layout.el []

        Nothing ->
            crops
                |> List.map
                    (\crop ->
                        crop
                            |> View.Crop.asCircle
                                (Layout.asButton
                                    { label = "Select Crop"
                                    , onPress = Just (onSelect (crop |> Just))
                                    }
                                )
                    )
                |> Layout.row []
    ]
        |> Layout.column []
