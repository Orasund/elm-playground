module GreenFields.View.Card exposing (view)

import Bag
import Color as C
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GreenFields.Data.Building as Building exposing (Building)
import GreenFields.Data.Effect as Effect
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.Data.Tile as Tile
import GreenFields.View.Color as Color
import Time exposing (Posix)


restoringCosts building =
    building
        |> Building.restoringCost
        |> Bag.toList
        |> (\list ->
                if list |> List.isEmpty then
                    Element.none

                else
                    "Costs "
                        ++ (list
                                |> List.map
                                    (\( name, n ) ->
                                        String.fromInt n
                                            ++ " "
                                            ++ name
                                    )
                                |> String.join ", "
                           )
                        |> Element.text
                        |> List.singleton
                        |> Element.paragraph []
           )


buildingCosts building =
    building
        |> Building.buildingCost
        |> Bag.toList
        |> (\list ->
                if list |> List.isEmpty then
                    Element.none

                else
                    "Costs "
                        ++ (list
                                |> List.map
                                    (\( name, n ) ->
                                        String.fromInt n
                                            ++ " "
                                            ++ name
                                    )
                                |> String.join ", "
                           )
                        |> Element.text
                        |> List.singleton
                        |> Element.paragraph []
           )


gives building =
    building
        |> Building.produces
        |> Bag.toList
        |> (\list ->
                if list |> List.isEmpty then
                    Element.none

                else
                    "Gives "
                        ++ (list
                                |> List.map
                                    (\( name, n ) ->
                                        String.fromInt n
                                            ++ " "
                                            ++ name
                                    )
                                |> String.join ", "
                           )
                        |> Element.text
                        |> List.singleton
                        |> Element.paragraph []
           )


effects building =
    building
        |> Building.effects
        |> List.map
            (Effect.toString
                >> Element.text
                >> List.singleton
                >> Element.paragraph []
            )


upgradedGives : { old : Building, new : Building } -> Element msg
upgradedGives args =
    Building.upgradedProduction
        { old = args.old, new = args.new }
        |> Bag.toList
        |> (\list ->
                if list |> List.isEmpty then
                    Element.none

                else
                    "Gives "
                        ++ (list
                                |> List.map
                                    (\( name, n ) ->
                                        String.fromInt n
                                            ++ " "
                                            ++ name
                                    )
                                |> String.join ", "
                           )
                        |> Element.text
                        |> List.singleton
                        |> Element.paragraph []
           )


view :
    { selected : Maybe ( Int, Int )
    , onRestore : msg
    , onBuild : Building -> msg
    , onDemolish : msg
    , game : Game
    , timestamp : Posix
    }
    -> Element msg
view arg =
    let
        attr =
            [ Element.alignLeft
            , Element.padding 16
            , Element.width <| Element.px 300
            , Element.height <| Element.px 400
            , Element.centerY
            , Border.rounded 16
            , Element.spacing 1
            ]
    in
    case arg.selected of
        Just pos ->
            case arg.game |> Game.getTile pos of
                Just tile ->
                    let
                        isOld =
                            tile |> Tile.isOld arg.timestamp

                        colors =
                            if isOld then
                                { background = Color.green
                                , border = Color.white
                                , font = Color.white
                                , link = Color.black
                                }

                            else
                                { background = Color.white
                                , border = Color.white
                                , font = Color.black
                                , link = Color.green
                                }
                    in
                    [ [ (if isOld then
                            "Decayed "

                         else
                            ""
                        )
                            ++ (tile.building
                                    |> Building.toString
                               )
                            |> Element.text
                      , "==========" |> Element.text
                      , " " |> Element.text
                      ]
                    , if isOld then
                        [ if arg.game |> Game.canRestore pos then
                            Input.button
                                [ colors.link
                                    |> C.toRgba
                                    |> Element.fromRgb
                                    |> Font.color
                                , Font.bold
                                ]
                                { onPress = Just arg.onRestore
                                , label = "[Restore]" |> Element.text
                                }

                          else
                            "[Restore]" |> Element.text
                        , restoringCosts tile.building
                        , gives tile.building
                        ]
                            ++ effects tile.building
                            ++ [ " " |> Element.text
                               , Input.button
                                    [ colors.link
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Font.color
                                    , Font.bold
                                    ]
                                    { onPress = Just arg.onDemolish
                                    , label = "[Demolish]" |> Element.text
                                    }
                               ]

                      else
                        tile.building
                            |> Building.upgrades
                            |> List.concatMap
                                (\building ->
                                    [ if arg.game |> Game.canBuild building then
                                        Input.button
                                            [ Color.green
                                                |> C.toRgba
                                                |> Element.fromRgb
                                                |> Font.color
                                            , Font.bold
                                            ]
                                            { onPress = arg.onBuild building |> Just
                                            , label = "[Build " ++ (building |> Building.toString) ++ "]" |> Element.text
                                            }

                                      else
                                        "[Build " ++ (building |> Building.toString) ++ "]" |> Element.text
                                    , buildingCosts building
                                    , upgradedGives
                                        { new = building, old = tile.building }
                                    ]
                                        ++ effects building
                                        ++ [ " " |> Element.text
                                           ]
                                )
                    ]
                        |> List.concat
                        |> Element.column
                            (attr
                                ++ [ colors.background
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Background.color
                                   , colors.border
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Border.color
                                   , Border.width 1
                                   , colors.font
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Font.color
                                   ]
                            )

                Nothing ->
                    Building.listTier1
                        |> List.concatMap
                            (\building ->
                                [ if arg.game |> Game.canBuild building then
                                    Input.button
                                        [ Color.black
                                            |> C.toRgba
                                            |> Element.fromRgb
                                            |> Font.color
                                        , Font.bold
                                        ]
                                        { onPress = arg.onBuild building |> Just
                                        , label = "[Build " ++ (building |> Building.toString) ++ "]" |> Element.text
                                        }

                                  else
                                    "[Build " ++ (building |> Building.toString) ++ "]" |> Element.text
                                , buildingCosts building
                                , gives building
                                ]
                                    ++ effects building
                                    ++ [ " " |> Element.text ]
                            )
                        |> Element.column
                            (attr
                                ++ [ Element.centerY
                                   , Color.green
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Background.color
                                   , Color.green
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Border.color
                                   , Border.width 1
                                   , Color.white
                                        |> C.toRgba
                                        |> Element.fromRgb
                                        |> Font.color
                                   ]
                            )

        Nothing ->
            "Click on a white square to view the details"
                |> Element.text
                |> List.singleton
                |> Element.paragraph [ Element.centerY ]
                |> Element.el
                    (attr
                        ++ [ Color.white
                                |> C.toRgba
                                |> Element.fromRgb
                                |> Font.color
                           ]
                    )
